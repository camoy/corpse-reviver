#lang s-exp corpse-reviver/private/lang/typed/base

(provide adder)

(: adder (-> Number Number))
(define (adder x)
  (add1 x))
