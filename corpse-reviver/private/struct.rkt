#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [provide-structs (-> syntax? definitions/c exports/c structs/c)]
  [require-structs (-> syntax? definitions/c exports/c structs/c)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require fancy-app
         lens
         mischief/for
         racket/list
         racket/match
         "data.rkt"
         "syntax.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; Symbol → (Syntax Definitions Exports → Structs)
;; Construct a structure containing information about provided or required
;; structs.
(define ((structs key) stx defns exports)
  (define props (syntax-property-values stx key))
  (define-values (local-fields names)
    (struct-local-fields props defns exports))
  (struct-all-fields local-fields names))

;; Syntax Definitions Exports → Structs
(define provide-structs (structs 'provide-struct))

;; Syntax Definitions Exports → Structs
(define require-structs (structs 'require-struct))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; util

;; [Listof Vector] Definitions Exports → Structs [Listof Symbol]
;; Construct a structs hash, only with local fields and contracts (not anything
;; from the super). Also return a list of struct names sorted by ancestry.
(define (struct-local-fields props defns exports)
  (define/for/fold ([result (hash)]
                    [parent-tree (hash)])
                   ([prop (in-list props)])
    (match-define (vector name-stx parent-stx pred-stx sels muts) prop)
    (define name (syntax-e name-stx))
    (define parent (and (identifier? parent-stx) (syntax-e parent-stx)))
    (define/for/lists (fields contracts)
                      ([sel (in-list sels)]
                       #:when (field-name name sel))
      (define ctc (hash-ref exports (syntax-e sel)))
      (values (field-name name sel)
              (chase-codomain defns ctc)))
    (values (hash-set result name (struct-data parent fields contracts))
            (hash-set parent-tree name (list parent))))
  (values result (topological-sort parent-tree)))

;; Structs [Listof Symbol] → Structs
;; Given a structs hash with local fields and a sorted list of struct names, fill
;; in the rest of the fields by appending the parent's fields and contracts.
(define (struct-all-fields local-fields names)
  (for/fold ([result local-fields])
            ([name (in-list names)])
    (define data (hash-ref result name))
    (define parent (struct-data-parent data))
    (define-values (parent-fields parent-contracts)
      (if parent
          (let ([parent-data (hash-ref result parent)])
            (values (struct-data-fields parent-data)
                    (struct-data-contracts parent-data)))
          (values null null)))
    (define data*
      (lens-transform/list
       data
       struct-data-fields-lens (append parent-fields _)
       struct-data-contracts-lens (append parent-contracts _)))
    (hash-set result name data*)))

;; Symbol Syntax → Symbol
;; Returns the field name associated with a selector.
(define (field-name name selector)
  (define pat
    (pregexp (format "^~a-(.+)$" (regexp-quote (symbol->string name)))))
  (define result
    (regexp-match pat (symbol->string (syntax-e selector))))
  (match result
    [(list _ fld) (string->symbol fld)]
    [_ #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require (only-in rackunit/chk chk)
           (submod "compile.rkt" test)
           rackunit)

  (define-values (i? r?)
    (values #'integer? #'real?))

  (define defns
    (hash 'a #`(-> child? #,i?)
          'b #`(-> parent? #,r?)))

  (define exports
    (hash 'child-c #'a
          'parent-p #'b))

  (define expected-structs
    (hash 'child (struct-data 'parent '(p c) (list r? i?))
          'parent (struct-data #f '(p) (list r?))))

  (define-values (local-fields names)
    (struct-local-fields
     (syntax-property-values ty-ty-server-expand 'provide-struct)
     defns exports))

  (test-case "provide-structs"
    (chk
     (provide-structs ty-ty-server-expand defns exports)
     expected-structs))

  (test-case "require-structs"
    (chk
     (require-structs ty-ut-client-expand defns exports)
     expected-structs))

  (test-case "struct-local-fields"
    (chk
     (values local-fields
             (hash 'child
                   (struct-data 'parent '(c) (list i?))
                   'parent
                   (struct-data #f '(p) (list r?)))
             names
             '(parent child))))

  (test-case "struct-all-fields"
    (chk
     (struct-all-fields local-fields names)
     expected-structs))

  (test-case "field-name"
    (chk
     (field-name 'stream #'stream-first) 'first
     #:f #:t (field-name 'stream #'foo-first)))
  )
