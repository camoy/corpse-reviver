#lang typed/racket/base

(provide (struct-out parent)
         (struct-out child)
         f)

(struct parent ([p : Integer]))
(struct child parent ([c : Integer]))

(: f (-> child child))
(define (f x) x)
