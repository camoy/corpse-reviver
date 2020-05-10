#lang racket/base

(require "server.rkt")

(define (probably-ok x)
  (if (< x 0.00001)
      (adder "bad")
      (adder 0)))

(probably-ok (random))
