#lang typed/racket/base

(require corpse-reviver/opaque)
(require/typed/opaque "a.rkt" [a Integer])
(require/typed/opaque "b.rkt" [b Integer])

(provide foo)

(define (foo)
  (+ a b))

(foo)
