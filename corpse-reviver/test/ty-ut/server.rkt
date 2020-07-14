#lang racket/base

(provide (struct-out parent)
         (struct-out child)
         f)

(struct parent (p))
(struct child parent (c))
(define (f x) x)
