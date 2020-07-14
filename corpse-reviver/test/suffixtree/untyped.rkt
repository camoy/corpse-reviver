#lang racket/base

(provide assert index?)

(require (only-in racket/unsafe/ops unsafe-fx>=))

(define (index? x) (and (fixnum? x) (unsafe-fx>= x 0) (fixnum? (* x 4))))

(define (assert v p)
  (unless (p v) (error 'assert))
  v)
