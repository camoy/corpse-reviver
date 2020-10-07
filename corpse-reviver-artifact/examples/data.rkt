#lang typed/racket/base

(provide random-adders)

(: random-adders : (-> Natural (Listof (-> Integer Integer))))
(define (random-adders n)
  (for/list ([i (in-range n)])
    (λ ([x : Integer]) (+ (random 10) x))))
