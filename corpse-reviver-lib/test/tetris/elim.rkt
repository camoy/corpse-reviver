#lang typed/racket

(require "base-types.rkt")
(require corpse-reviver/require-typed-check)
(require/typed "bset.rkt"
   [blocks-move (-> Real Real BSet BSet)]
   [full-row? (-> BSet Natural Boolean)]
   [blocks-union (-> BSet BSet BSet)]
   [blocks-row (-> BSet Real BSet)])
(require/typed "consts.rkt"
  [board-height Integer])

;; Eliminate all full rows and shift down appropriately.
(: eliminate-full-rows (-> BSet BSet))
(define (eliminate-full-rows bs)
  (elim-row bs board-height 0))

(: elim-row (-> BSet Integer Integer BSet))
(define (elim-row bs i offset)
  (cond [(< i 0) empty]
        [(full-row? bs i)   (elim-row bs (sub1 i) (add1 offset))]
        [else (blocks-union (elim-row bs (sub1 i) offset)
                            (blocks-move 0 offset (blocks-row
                                                   bs i)))]))
(provide
 eliminate-full-rows)
