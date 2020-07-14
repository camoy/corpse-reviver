#lang typed/racket/base

(require/typed "a.rkt" [a Integer])
(require/typed "b.rkt" [b Integer])

(provide foo)

(define (foo)
  (+ a b))

(foo)
