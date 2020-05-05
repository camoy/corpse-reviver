#lang racket/base

(require pict)
(provide pict? absz (struct-out my-box) number?)

(struct my-box (x))

(define bad-for-scv (make-parameter #f))

(define (absz x)
  (if (> x 0) x (- x)))
