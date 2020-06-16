#lang racket

(require "aux.rkt" "world.rkt" "bset.rkt" "data.rkt")

(define (world0)
  (world (list-pick-random tetras) empty))

(define (game-over? w)
  (blocks-overflow? (world-blocks w)))

(define (replay w0 hist)
  (for/fold ([w w0]) ([e hist])
    (match e
      [`(on-key ,(? string? ke)) (world-key-move w ke)]
      [`(on-tick) (next-world w)]
      [`(stop-when)
       (game-over? w)
       w]))
  (void))

(define DATA (with-input-from-file "../base/tetris-hist.rktd" read))
(define LOOPS 2)

(define (main raw)
  (define w0 (world0))
  (if (list? raw)
    (for ((_i (in-range LOOPS)))
      (replay w0 raw))
    (error "bad input")))

(time (main DATA))
