#lang racket

(require "data.rkt"
         "bset.rkt"
         "consts.rkt")

;; eliminate-full-rows : BSet -> BSet
;; Eliminate all full rows and shift down appropriately.
(define (eliminate-full-rows bs)
  (elim-row bs board-height 0))

(define (elim-row bs i offset)
  (cond [(< i 0) empty]
        [(full-row? bs i)   (elim-row bs (sub1 i) (add1 offset))]
        [else (blocks-union (elim-row bs (sub1 i) offset)
                            (blocks-move 0 offset (blocks-row
                                                   bs i)))]))
(provide
 eliminate-full-rows)
