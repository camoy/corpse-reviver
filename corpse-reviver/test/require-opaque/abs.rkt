#lang typed/racket/base

(provide absz (struct-out my-box))

(struct my-box ([x : Real]))

(: bad-for-scv (Parameter Boolean))
(define bad-for-scv (make-parameter #f))

(: absz (-> Real Number))
(define (absz x)
  (if (> x 0) x (- x)))
