#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide syntax-property*
         syntax-property-self*
         syntax-property-self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; Syntax → Syntax
;; Helper for calling syntax-property-self from Typed Racket source.
(define-syntax-rule (syntax-property* key v e ...)
  (syntax-property (let () e ...) key v))

;; Syntax → Syntax
;; Helper for calling syntax-property-self from Typed Racket source.
(define-syntax-rule (syntax-property-self* key e ...)
  (syntax-property-self (let () e ...) key))

;; Syntax Symbol → Syntax
;; Associates the syntax itself with the given key.
(define (syntax-property-self stx key)
  (syntax-property stx key stx))
