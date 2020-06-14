#lang typed/racket/base

(require "abs.rkt")

(: boxed-x my-box)
(define boxed-x (my-box -3))

(displayln (absz (my-box-x boxed-x)))
