#lang racket/base

(provide foo)

(require math/statistics)

(define (foo x)
  (mean x))
