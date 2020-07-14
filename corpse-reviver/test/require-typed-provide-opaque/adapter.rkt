#lang typed/racket/base

(require/typed/provide "abs.rkt"
  [absz (-> Real Number)]
  [#:struct my-box ([x : Real])])

(: boxed-x my-box)
(define boxed-x (my-box -3))

(displayln (absz (my-box-x boxed-x)))
