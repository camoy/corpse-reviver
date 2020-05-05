#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide register-contracts!
         require/typed
         require/typed/provide
         (rename-out [-provide provide])
         (rename-out [-provide scv-cr:provide])
         (rename-out [provide racket:provide])
         define-predicate
         make-predicate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/function
                     racket/list
                     racket/match
                     racket/struct-info
                     racket/syntax
                     syntax/parse
                     syntax/strip-context
                     "../../syntax.rkt"
                     "../../data.rkt")
         (only-in soft-contract/fake-contract [provide scv:provide]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils

(begin-for-syntax
  ;; Contracts
  (define contracts #f)

  ;; [Listof Syntax] → Boolean
  ;; Takes in the forms of a provide and determines if they should be excluded
  ;; (since they have already been exported by SCV-CR).
  (define (exclude-outs xs)
    (define exports (bundle-exports (contracts-provide contracts)))
    (define structs (hash-keys (bundle-structs (contracts-provide contracts))))
    (define (should-exclude? form)
      (match (syntax->datum form)
        [`(struct-out ,name) (member name structs)]
        [`(contract-out ,out ...) #f]
        [`(f:contract-out ,out ...) #f]
        [(? symbol? x) (hash-has-key? exports x)]
        [x (error 'exclude-outs "unrecognized provide form ~a" x)]))
    (filter (negate should-exclude?) xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros

;; Registers the contracts for use in expansion.
(define-syntax (register-contracts! stx)
  (syntax-parse stx
    [(_ ?ctcs)
     (set! contracts (syntax-property #'?ctcs 'payload))
     #'(void)]))

;; Disable require/typed within an analysis (since this will always be provided
;; by SCV-CR via a require/safe submodule).
(define-syntax (require/typed stx)
  (syntax-parse stx
    [(_ ?m:expr ?x:clause ...)
     (if (syntax-property #'?m 'opaque)
         (replace-context stx #'(begin ?x.opaque ...))
         #'(void))]))

;; Disable require/typed/provide within an analysis for the same reason as above,
;; but provide the bindings since Typed Racket will not give those to the provide
;; bundle on a require/typed/provide.
(define-syntax (require/typed/provide stx)
  (syntax-parse stx
    [(_ ?m:expr ?x:clause ...)
     (if (syntax-property #'?m 'opaque)
         (replace-context stx #'(begin (provide ?x.out ...) ?x.opaque ...))
         (replace-context stx #'(provide ?x.out ...)))]))

;; Provide but excluding identifiers that were already exported by SCV-CR with
;; contracts.
(define-syntax (-provide stx)
  (syntax-parse stx
    [(_ ?x ...)
     #:with (?x* ...) (exclude-outs (attribute ?x))
     #'(scv:provide ?x* ...)]))

;; Defines a predicate based on a type.
(define-syntax (define-predicate stx)
  (syntax-parse stx
    [(_ ?x:id _)
     #:with ?defn (hash-ref (contracts-predicates contracts) (syntax->datum stx))
     #'(define ?x ?defn)]))

;; Returns a predicate based on a type.
(define-syntax (make-predicate stx)
  (hash-ref (contracts-predicates contracts) (syntax->datum stx)))
