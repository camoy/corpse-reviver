#lang typed/racket/base

(provide f)

(: f (-> Natural * Natural))
(define (f . xs) 0)
