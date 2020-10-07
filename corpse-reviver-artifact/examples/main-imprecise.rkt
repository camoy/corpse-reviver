#lang racket/base

(require "data.rkt")

(define iterations 1000)
(define n 5000)

(time
 (for ([i (in-range iterations)])
   (for/sum ([f (in-list (random-adders n))])
     (f (if (string<=? "a" "b")
            (random 10)
            "")))))
