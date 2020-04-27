#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide scv-ignore:provide
         require/define)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/function
                     racket/list
                     racket/struct-info
                     racket/syntax
                     syntax/parse
                     "../syntax.rkt"
                     "../log.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros

;; Syntax → Syntax
;; Provide form that is ignored by SCV.
(define-syntax (scv-ignore:provide stx)
  (syntax-parse stx
    [(_ x ...)
     (scv-ignore #'(provide x ...))]))

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
