#lang racket/base

(provide adder)

(define (adder x)
  (format "~a" (add1 x)))
