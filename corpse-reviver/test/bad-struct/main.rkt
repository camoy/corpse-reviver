#lang racket/base

(provide (struct-out posn))

(require "server.rkt")

(define (probably-ok x)
  (if (< x 0.0001)
      (posn-adder-x (posn "bad" 0))
      (posn-adder-x (posn 0 0))))

(probably-ok (random))
