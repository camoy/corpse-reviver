#lang typed/racket/base

(provide g)

(require/typed "server.rkt"
  [#:struct parent ([p : Integer])]
  [#:struct (child parent) ([c : Integer])]
  [f (-> child child)])

(: g (-> Integer child))
(define (g x)
  (f (child x x)))
