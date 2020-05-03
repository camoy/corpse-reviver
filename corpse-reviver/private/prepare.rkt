#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [prepare (-> syntax? contracts? syntax?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base)
         racket/function
         racket/match
         racket/set
         syntax/parse
         syntax/strip-context
         "data.rkt"
         "syntax.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "pre-transformer" bindings

;; Contracts Syntax → Syntax
;; Disable require/typed within an analysis (since this will always be provided
;; by SCV-CR via a require/safe submodule).
(define (-require/typed _ stx)
  #'(void))

;; Contracts Syntax → Syntax
;; Disable require/typed/provide within an analysis for the same reason as above,
;; but provide the bindings since Typed Racket will not give those to the provide
;; bundle on a require/typed/provide.
(define (-require/typed/provide ctcs stx)
  (syntax-parse stx
    [(_ _ ?x:clause ...)
     (-provide ctcs #'(provide ?x.out ...))]))

;; Contracts Syntax → Syntax
;; Provide but excluding identifiers that were already exported by SCV-CR with
;; contracts.
(define (-provide ctcs stx)
  (syntax-parse stx
    [(_ ?x ...)
     #:with (?x* ...) (exclude-outs ctcs (attribute ?x))
     #'(provide ?x* ...)]))

;; Contracts Syntax → Syntax
;; Defines a predicate based on a type.
(define (-define-predicate ctcs stx)
  (syntax-parse stx
    [(_ ?x:id _)
     #:with ?defn (-make-predicate ctcs stx)
     #'(define ?x ?defn)]))

;; Contracts Syntax → Syntax
;; Retunrns a predicate based on a type.
(define (-make-predicate ctcs stx)
  (hash-ref (contracts-predicates ctcs) (syntax->datum stx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Contracts [Listof Syntax] → Boolean
;; Takes in the forms of a provide and determines if they should be excluded
;; (since they have already been exported by SCV-CR).
(define (exclude-outs ctcs xs)
  (define exports (bundle-exports (contracts-provide ctcs)))
  (define structs (hash-keys (bundle-structs (contracts-provide ctcs))))
  (define (should-exclude? form)
    (match (syntax->datum form)
      [`(struct-out ,name) (member name structs)]
      [`(contract-out ,_ ...) #f]
      [(? symbol? x) (hash-has-key? exports x)]
      [x (error 'exclude-outs "unrecognized provide form ~a" x)]))
  (filter (negate should-exclude?) xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pre-expander

(define-syntax (make-syntax-env stx)
  (syntax-case stx ()
    [(_ [?old ?new] ...)
     #'(hash (~@ '?new ?old) ...)]))

;; [Hash Symbol (→ Syntax Syntax)]
;; Tables that maps symbols (form names) to "pre-transformer" bindings.
(define syntax-env
  (make-syntax-env
   [-require/typed require/typed]
   [-require/typed/provide require/typed/provide]
   [-provide provide]
   [-define-predicate define-predicate]
   [-make-predicate make-predicate]))

;; Syntax Contracts → Syntax
;; Apply pre-expansion. This is necessary because SCV derives some information
;; from the surface syntax of a program. These implement the sundry changes so
;; SCV doesn't complain.
(define (prepare stx ctcs)
  (define stx*
    (let go ([stx stx])
      (cond
        [(syntax? stx)
         (datum->syntax stx (go (syntax-e stx)) stx stx)]
        [(list? stx)
         (match stx
           [(list-rest x xt)
            (define x* (syntax-e x))
            (if (hash-has-key? syntax-env x*)
                ((hash-ref syntax-env x*) ctcs (datum->syntax #f stx))
                (map go stx))]
           [x x])]
        [(pair? stx)
         (cons (go (car stx)) (go (cdr stx)))]
        [else stx])))
  (strip-context* stx*))
