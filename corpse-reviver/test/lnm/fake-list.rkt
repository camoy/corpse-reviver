#lang racket/base

(provide sort*)
(define (sort* xs lt key)
  (sort xs lt #:key key))
