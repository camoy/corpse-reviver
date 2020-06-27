#lang racket/base

(provide (struct-out posn))

(require "server.rkt")

(define (definitely-ok x)
  (posn-adder-x (posn 0 0)))

(definitely-ok (random))
