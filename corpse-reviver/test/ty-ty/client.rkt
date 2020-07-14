#lang typed/racket/base

(provide g)

(require "server.rkt")

(: g (-> Integer child))
(define (g x)
  (f (child x x)))
