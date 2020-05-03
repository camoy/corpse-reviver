#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (rename-out [-require/typed require/typed])
         #;(rename-out [-require/typed/provide require/typed/provide])
         register-unsafe-hash!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/modresolve
                     syntax/parse
                     "../../syntax.rkt")
         racket/require)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros

(begin-for-syntax
  (define unsafe-hash #f))

;; Registers the unsafe hash for use in the require form.
(define-syntax (register-unsafe-hash! stx)
  (syntax-parse stx
    [(_ h)
     (set! unsafe-hash (syntax->datum #'h))
     #'(void)]))

;; Rewrite require to import safe bindings from the uncontracted submodule.
(define-syntax (-require/typed stx)
  (define (unsafe-clause m id)
    (define m-path (path->string (resolve-module-path (syntax->datum m))))
    (define unsafes (hash-ref unsafe-hash m-path))
    (member (syntax->datum id) unsafes))
  (syntax-parse stx
    [(_ m c:clause ...)
     #:with (?c-unsafe ...)
     (filter (λ (x) (unsafe-clause #'m (attr x x))) (syntax-e #'(c ...)))
     #:with (?c-safe ...)
     (filter (λ (x) (not (unsafe-clause #'m (attr x x)))) (syntax-e #'(c ...)))
     #'(begin
         (require/typed ?c-unsafe ...)
         (unsafe-require/typed ?c-safe ...))]))
