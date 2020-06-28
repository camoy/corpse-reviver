#lang racket/base

(provide assert)

(define (assert v p)
  (unless (p v) (error 'assert))
  v)
