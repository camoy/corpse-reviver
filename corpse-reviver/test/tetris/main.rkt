#lang typed/racket

(require "base-types.rkt")
(require scv-cr/require-typed-check)
(require/typed/check "aux.rkt"
  [list-pick-random (-> (Listof Tetra) Tetra)]
  [tetras (Listof Tetra)])
(require/typed/check "bset.rkt"
   [blocks-overflow? (-> BSet Boolean)])
(require/typed/check "world.rkt"
  [world-key-move (-> World String World)]
  [next-world (-> World World)])

(define (world0)
  (world (list-pick-random tetras) empty))

(: game-over? (-> World Boolean))
(define (game-over? w)
  (blocks-overflow? (world-blocks w)))

(: replay (-> World (Listof Any) Void))
(define (replay w0 hist)
  (for/fold ([w : World w0])
            ([e hist])
    (match e
      [`(on-key ,(? string? ke)) (world-key-move w ke)]
      [`(on-tick) (next-world w)]
      [`(stop-when)
       (game-over? w)
       w]))
  (void))

(define DATA (with-input-from-file "../base/tetris-hist.rktd" read))
(define LOOPS 1)

(: main (-> Any Void))
(define (main raw)
  (define w0 (world0))
  (if (list? raw)
    (for ((_i (in-range LOOPS)))
      (replay w0 raw))
    (error "bad input")))

(time (main DATA))
