#lang typed/racket/base

(provide adder)

(: adder (-> Real Real))
(define (adder x)
  (+ x 1))
