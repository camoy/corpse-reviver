#lang typed/racket/base

(require "structs-adapted.rkt")

(provide f)

(: f (-> Exp Exp))
(define (f x) x)
