#lang typed/racket/base

(provide
 min  ;(number? number? . -> . number?)]
 max  ;(number? number? . -> . number?)]
 abs  ;(number? . -> . number?)]
 sqrt ;(number? . -> . number?)]
 sqr  ;(number? . -> . number?)]
 msqrt
)

;; =============================================================================

(: min (-> Real Real Real))
(define (min x y) (if (<= x y) x y))
(: max (-> Real Real Real))
(define (max x y) (if (>= x y) x y))
(: abs (-> Real Real))
(define (abs x) (if (>= x 0) x (- 0 x)))
(: sqr (-> Real Real))
(define (sqr x) (* x x))
(: msqrt (-> Real Real))
(define (msqrt x) (assert (sqrt x) real?))
