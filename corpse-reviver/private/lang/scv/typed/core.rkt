#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (rename-out [-require require])
         require/typed
         require/typed/provide

         (rename-out [-provide provide])
         (rename-out [-provide scv-cr:provide])

         (rename-out [struct racket:struct])
         define-predicate
         make-predicate
         require/define)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/function
                     racket/list
                     racket/match
                     racket/struct-info
                     racket/syntax
                     soft-contract/parse/utils
                     syntax/parse
                     syntax/strip-context
                     threading
                     "../../../data.rkt"
                     "../../../struct.rkt"
                     "../../../syntax.rkt")
         (only-in soft-contract/fake-contract [provide scv:provide])
         "../common.rkt")

;; Disable require/typed within an analysis (since this will always be provided
;; by SCV-CR via a require/safe submodule). The only exception are clauses marked
;; with opaque. These are not imported so we have to define them as opaque types.
(define-syntax (require/typed stx)
  (syntax-parse stx
    [(_ ?m:expr ?x:clause ...)
     (replace-context stx #'(begin ?x.define ...))]))

;; Disable require/typed/provide within an analysis for the same reason as above,
;; but provide the bindings since Typed Racket will not give those to the provide
;; bundle on a require/typed/provide.
(define-syntax (require/typed/provide stx)
  (syntax-parse stx
    [(_ ?m:expr ?x:clause ...)
     (define provides (with-syntax-source stx #'(provide ?x.out ... ...)))
     (replace-context stx #`(begin ?x.define ... #,provides))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide macro

(begin-for-syntax
  ;; Syntax [Listof Syntax] → Boolean
  ;; Takes in the forms of a provide and determines if they should be excluded
  ;; (since they have already been exported by SCV-CR).
  (define (exclude-outs stx xs)
    (define bundle (contracts-provide (current-contracts stx)))
    (define exports (bundle-exports bundle))
    (define structs (bundle-structs bundle))
    (define struct-names (hash-keys structs))
    (define struct-exports (structs-exports structs))
    (define (should-exclude? form)
      (match (syntax->datum form)
        [`(struct-out ,name) (member name struct-names)]
        [`(contract-out ,out ...) #f]
        [`(f:contract-out ,out ...) #f]
        [(? symbol? x) (or (hash-has-key? exports x)
                           (member x struct-exports))]
        [x (error 'exclude-outs "unrecognized provide form ~a" x)]))
    (filter (negate should-exclude?) xs))
  )

;; Provide but excluding identifiers that were already exported by SCV-CR with
;; contracts.
(define-syntax (-provide stx)
  (syntax-parse stx
    [(_ ?x ...)
     #:with (?x* ...) (exclude-outs stx (attribute ?x))
     #'(scv:provide ?x* ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predicate macros

(begin-for-syntax
  ;; Syntax → Syntax
  ;; Get predicate from syntax.
  (define (get-predicate stx)
    (replace-context
     stx
     (hash-ref (contracts-predicates (current-contracts stx))
               (syntax->datum stx))))
  )

;; Defines a predicate based on a type.
(define-syntax (define-predicate stx)
  (syntax-parse stx
    [(_ ?x:id _)
     #:with ?defn (get-predicate stx)
     #'(define ?x ?defn)]))

;; Returns a predicate based on a type.
(define-syntax make-predicate get-predicate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; redefinition
;;
;; Suppose a module imports an identifier, say a predicate that defines an opaque
;; type. Use of that opaque type in other modules will point the predicate's
;; binding to that module. However, when we do elaboration, that module's imports
;; will be pushed into a require/contracts submodule, invalidating that
;; reference. To solve this we re-define single (non-struct) imports in the
;; current module if it was imported from require/contracts. Some scope nonsense
;; is necessary to prevent this from clashing with the existing import of
;; require/typed.

(begin-for-syntax
  ;; Scope for redefinition import.
  (define sc (make-syntax-introducer))

  ;; Syntax → Syntax
  ;; Get the name of a predicate.
  (define (predicate-name id)
    (format-id id "~a?" id))

  ;; Syntax → Syntax
  ;; Get the real predicate identifier.
  (define (predicate-id id)
    (third (extract-struct-info (syntax-local-value (sc id))))))

;; Syntax → Syntax
;; Require, but redefine the given imports and struct exports in this module.
(define-syntax (require/define stx)
  (syntax-parse stx
    [(_ m (imp:id ...) (s-imp:id ...))
     #:with [s-imp? ...] (map predicate-name (attribute s-imp))
     #:with m* (sc #'m)
     #'(begin
         (require (only-in m* imp ... s-imp? ...)
                  (except-in m imp ... s-imp? ...))
         (redefine (imp ...) (s-imp ...)))]))

;; Syntax → Syntax
;; Do the actual redefinition (this time we have the struct info).
(define-syntax (redefine stx)
  (syntax-parse stx
    [(_ (imp:id ...) (s-imp:id ...))
     #:with [s-imp? ...] (map predicate-name (attribute s-imp))
     #:with [s-imp?* ...] (map (compose sc predicate-id) (attribute s-imp))
     #:with [imp* ...] (map sc (attribute imp))
     #'(define-values (imp ... s-imp? ...)
         (values imp* ... s-imp?* ...))]))