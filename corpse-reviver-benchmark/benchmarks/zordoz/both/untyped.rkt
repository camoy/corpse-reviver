#lang racket/base

(provide index?)

(require (only-in racket/unsafe/ops unsafe-fx>=))

(define (index? x) (and (fixnum? x) (unsafe-fx>= x 0) (fixnum? (* x 4))))
