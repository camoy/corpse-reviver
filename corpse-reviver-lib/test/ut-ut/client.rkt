#lang racket/base

(provide g)

(require "server.rkt")

(define (g x)
  (f (child x x)))
