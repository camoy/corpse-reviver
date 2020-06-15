#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide require/opaque
         require/typed/opaque
         require/typed/provide/opaque)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse
                     syntax/strip-context
                     soft-contract/parse/utils
                     "private/syntax.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros

(define-syntax (require/opaque stx)
  (syntax-parse stx
    [(_ m x:clause ...)
     #:with m* (syntax-property #'m 'opaque #t)
     (with-syntax-source stx
       (replace-context stx #'(require/typed m* x ...)))]))

(define-syntax (require/typed/opaque stx)
  (syntax-parse stx
    [(_ m x:clause ...)
     #:with m* (syntax-property #'m 'opaque #t)
     (with-syntax-source stx
       (replace-context stx #'(require/typed m* x ...)))]))

(define-syntax (require/typed/provide/opaque stx)
  (syntax-parse stx
    [(_ m x:clause ...)
     #:with m* (syntax-property #'m 'opaque #t)
     (with-syntax-source stx
       (replace-context stx #'(require/typed/provide m* x ...)))]))
