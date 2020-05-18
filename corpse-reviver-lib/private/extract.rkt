#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [make-contracts (-> syntax? contracts?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require mischief/for
         racket/contract
         racket/match
         racket/sequence
         racket/set
         syntax/parse
         "data.rkt"
         "munge.rkt"
         "struct.rkt"
         "syntax.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; Syntax → Contracts
;; Returns a contracts struct with the information for provide and require
;; contracts.
(define (make-contracts stx)
  (contracts (make-bundle stx 'provide)
             (make-bundle stx 'require)
             (make-libs stx)
             (make-opaques stx)
             (make-predicates stx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bundle

;; Syntax [Or 'require 'provide] → Bundle
;; Constructs a bundle from syntax and a syntax property key.
(define (make-bundle stx key)
  (define/for/fold ([defns   (hash)]
                    [exports (hash)])
                   ([stx (syntax-property-values stx key)])
    (values (make-definitions stx defns)
            (make-exports stx exports)))
  (define this-structs (structs key stx defns exports))
  (bundle defns exports this-structs))

;; Syntax Definitions → Definitions
;; Updates definitions to include those defined by the syntax.
(define (make-definitions stx defns)
  (syntax-parse stx
    #:literals (begin)
    [(begin ?d:definition ... (~optional _:export))
     (for/fold ([defns defns])
               ([name (in-syntax #'(?d.name ...))]
                [defn (in-syntax #'(?d.defn ...))])
       (hash-set defns
                 (syntax-e (or (lifted->l name) name))
                 (munge name defn)))]
    [_ defns]))

;; Syntax Exports → Exports
;; Updates the exports to include exports defined by the syntax.
(define (make-exports stx exports)
  (syntax-parse stx
    #:literals (begin)
    [(~or (begin _:definition ... ?e:export) ?e:import)
     (hash-set exports (syntax-e #'?e.name) (munge #'?e.name #'?e.contract))]
    [_ exports]))

;; Syntax → [Hash Any Syntax]
;; Returns a mapping from predicate syntax (as an sexp) to their definition.
(define (make-predicates stx)
  (for/hash ([v (in-set (syntax-property-values stx 'make-predicate))])
    (match-define (vector x y) v)
    (values (syntax->datum x) (lifted->l y))))

;; Syntax → List
;; Returns a list of unique values from the syntax of the syntax property.
(define (make-libs stx)
  (define dont-import-lib (set 'racket/base))
  (set->list
   (set-subtract
    (for/set ([x (in-set (syntax-property-values stx 'lib))]
              #:unless (syntax-property x 'opaque))
      (syntax-e x))
    dont-import-lib)))

;; Syntax → [Listof Syntax]
;; Returns a list of syntax for defining opaque imports.
(define (make-opaques stx)
  (for/lists (done result #:result result)
             ([x (in-set (syntax-property-values stx 'lib))]
              #:when (syntax-property x 'opaque)
              #:when (not (member (syntax-e x) done)))
    (values (syntax-e x)
            (syntax-property x 'opaque))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax classes

;; Syntax-Class
;; Class for a contract definition.
(define-syntax-class definition
  #:literals (define-values define)
  (pattern (define name:id defn:expr))
  (pattern (define-values (name:id) defn:expr)))

;; Syntax-Class
;; Class for an export of a binding with a contract.
(define-syntax-class export
  #:datum-literals (define-module-boundary-contract)
  (pattern (define-module-boundary-contract name:id _ contract:expr _ ...)))

;; Syntax-Class
;; Class for an import of a binding with a contract.
(define-syntax-class import
  #:datum-literals (begin require only-in define-ignored)
  (pattern (begin
             (require (only-in _ (name:id _)))
             _
             (define-ignored _ (_ contract:expr _ _ _ _ _)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           racket/function
           racket/hash
           "util.rkt"
           "../test/expand.rkt")

  ;; [Hash Any Syntax] → [Hash Any SExpr]
  ;; Converts every value with syntax->datum.
  (define (hash-values-syntax->datum h)
    (for/hash ([(k v) (in-hash h)])
      (values k (syntax->datum v))))

  ;; SExpr SExpr → [Assoc-List Symbol Symbol]
  ;; Returns an associative list that matches up contract definitions in the
  ;; first syntax with those of the other.
  (define (contract-assoc e1 e2)
    (let go ([e1 e1]
             [e2 e2])
      (cond
        [(pair? e1)
         (set-union (go (car e1) (car e2)) (go (cdr e1) (cdr e2)))]
        [(and (symbol? e1) (symbol->number e1))
         (list (cons e1 e2))]
        [else null])))

  ;; Definitions Definitions [Hash Symbol Symbol] → [Hash Symbol Symbol]
  ;; Generates a new mapping between contract definitions based on what was
  ;; reachable from prev.
  (define (contract-hash-step defns1 defns2 prev)
    (make-immutable-hash
     (for/append ([(v v*) (in-hash prev)])
       (define-values (d d*)
         (values (hash-ref defns1 v (const #f))
                 (hash-ref defns2 v* (const #f))))
       (if (and d d*)
           (contract-assoc d d*)
           null))))

  ;; Definitions Exports Definitions Exports → [Hash Symbol Symbol]
  ;; Constructs a hash between "equivalent" contract identifiers.
  (define (contract-hash defns1 exports1 defns2 exports2)
    (define init
      (for/hash ([(k v) (in-hash exports1)])
        (values v (hash-ref exports2 k))))
    (let go ([prev init])
      (define next
        (hash-union prev
                    (contract-hash-step defns1 defns2 prev)
                    #:combine (λ (_ x) x)))
      (if (equal? prev next) prev (go next))))

  ;; [Hash Symbol Symbol] → (SExpr SExpr → Boolean)
  ;; Returns if the two contracts represented by e1 and e2 (as s-exprs) are the
  ;; same modulo the given renaming hash.
  (define ((contract-equal? ctc-hash) e1 e2)
    (let go ([e1 e1]
             [e2 e2])
      (cond
        [(pair? e1)
         (and (go (car e1) (car e2)) (go (cdr e1) (cdr e2)))]
        [(symbol? e1)
         (or (equal? e1 e2) (equal? (hash-ref ctc-hash e1) e2))]
        [else (equal? e1 e2)])))

  ;; Definitions Exports Definitions Exports → Any
  ;; Checks to make sure that all the definitions are equivalent.
  (define (chk-defns defns1 exports1 defns2 exports2)
    (define ctc-hash
      (contract-hash defns1 exports1 defns2 exports2))
    (define ctc-equal? (contract-equal? ctc-hash))
    (for ([(k v) (in-hash defns1)])
      (define v* (hash-ref defns2 (hash-ref ctc-hash k)))
      (chk #:eq ctc-equal? v v*)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define streams-defns
    #hash((g15 . exact-nonnegative-integer?)
          (g16 . (or/c g15))
          (g17 . (lambda (x) (stream? x)))
          (g18 . (-> g17))
          (g19 . struct-type?)
          (g20 . (λ (x) #f))
          (g21 . any/c)
          (g22 . (quote #t))
          (g23 . (quote #f))
          (g24 . (or/c g22 g23))
          (g25 . (-> g21 (values g24)))
          (g26 . (or/c g20 g25))
          (g27 . (-> (values g17)))
          (g28 . (listof g16))
          (generated-contract11 . (-> g16 g18 (values g17)))
          (generated-contract12 . (-> g17 g16 (values g16)))
          (generated-contract13 . (-> g17 g16 (values g28)))
          (generated-contract14 . (-> g17 (values g16 g17)))
          (generated-contract5 . (-> g16 g18 (values g17)))
          (generated-contract6 . g19)
          (generated-contract7 . g26)
          (generated-contract8 . (-> g17 (values g27)))
          (generated-contract9 . (-> g17 (values g16)))))

  (define streams-exports
    #hash((make-stream . generated-contract5)
          (stream . generated-contract11)
          (stream-first . generated-contract9)
          (stream-get . generated-contract12)
          (stream-rest . generated-contract8)
          (stream-take . generated-contract13)
          (stream-unfold . generated-contract14)
          (stream? . generated-contract7)
          (struct:stream . generated-contract6)))

  (define main-defns
   #hash((g14 . exact-nonnegative-integer?)
         (g15 . (or/c g14))
         (g16 . (lambda (x) (stream? x)))
         (g17 . (-> (values g16)))
         (g18 . (-> g16))
         (g19 . (listof g15))
         (l33 . (-> g15 g17 (values g16)))
         (l35 . (-> g16 (values g15)))
         (l37 . (-> g16 (values g18)))
         (l39 . (-> g15 g17 (values g16)))
         (l41 . (-> g16 (values g15 g16)))
         (l43 . (-> g16 g15 (values g15)))
         (l45 . (-> g16 g15 (values g19)))))

  (define main-exports
    #hash((make-stream . l39)
          (stream . l33)
          (stream-first . l35)
          (stream-get . l43)
          (stream-rest . l37)
          (stream-take . l45)
          (stream-unfold . l41)
          (stream? . (or/c (λ (x) #f) (-> any/c boolean?)))))

  (match-define (bundle prov-defns prov-exports prov-structs)
    (make-bundle streams-expand 'provide))

  (match-define (bundle req-defns req-exports req-structs)
    (make-bundle sieve-main-expand 'require))

  (with-chk (['name "make-bundle"])
    (define stream-data (hash-ref prov-structs 'stream))
    (define main-data (hash-ref req-structs 'stream))
    (chk (struct-data-fields stream-data) '(first rest))
    (chk (struct-data-fields main-data) '(first rest)))

  (with-chk (['name "make-definitions"])
    (define-values (prov-defns* prov-exports*)
      (values (hash-values-syntax->datum prov-defns)
              (hash-values-syntax->datum prov-exports)))
    (define-values (req-defns* req-exports*)
      (values (hash-values-syntax->datum req-defns)
              (hash-values-syntax->datum req-exports)))
    (chk-defns streams-defns streams-exports prov-defns* prov-exports*)
    (chk-defns main-defns main-exports req-defns* req-exports*))

  (with-chk (['name "make-exports"])
    (chk (hash-keys streams-exports) (hash-keys prov-exports)))

  (with-chk (['name "make-predicates"])
    (define predicate-hash (make-predicates predicate-server-expand))
    (chk
     #:t (hash-has-key? predicate-hash '(define-predicate is-number*? Integer))
     #:t (hash-has-key? predicate-hash '(make-predicate Number))))

  (with-chk (['name "make-opaques"])
    (define x (syntax-property #'x 'opaque #'(define foo "foo")))
    (define y (syntax-property #'y 'opaque #'(define bar "bar")))
    (define z #`(begin #,(syntax-property #'1 'lib x)
                       #,(syntax-property #'2 'lib x)
                       #,(syntax-property #'3 'lib y)))
    (chk
     #:eq set=?
     (map syntax->datum (make-opaques z))
     '((define bar "bar") (define foo "foo"))))
  )
