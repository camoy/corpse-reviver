#lang racket/base

(provide absz (struct-out my-box))

(struct my-box (x))

(define bad-for-scv (make-parameter #f))

(define (absz x)
  (if (> x 0) x (- x)))
