#lang racket/base

(provide
 min  ;(number? number? . -> . number?)]
 max  ;(number? number? . -> . number?)]
 abs  ;(number? . -> . number?)]
 msqrt ;(number? . -> . number?)]
 sqr  ;(number? . -> . number?)]
)

;; =============================================================================

(define (min x y) (if (<= x y) x y))
(define (max x y) (if (>= x y) x y))
(define (abs x) (if (>= x 0) x (- 0 x)))
(define (sqr x) (* x x))
(define (msqrt x) (sqrt x))
