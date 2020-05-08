#lang racket

(require "data.rkt")

;; block=? : Block Block -> Boolean
;; Determines if two blocks are the same (ignoring color).
(define (block=? b1 b2)
  (and (= (block-x b1) (block-x b2))
       (= (block-y b1) (block-y b2))))

;; block-move : Number Number Block -> Block
(define (block-move dx dy b)
  (block (+ dx (block-x b))
         (+ dy (block-y b))
         (block-color b)))

;; block-rotate-ccw : Posn Block -> Block
;; Rotate the block 90 counterclockwise around the posn.
(define (block-rotate-ccw c b)
  (block (+ (posn-x c) (- (posn-y c) (block-y b)))
         (+ (posn-y c) (- (block-x b) (posn-x c)))
         (block-color b)))

;; block-rotate-cw : Posn Block -> Block
;; Rotate the block 90 clockwise around the posn.
(define (block-rotate-cw c b)
  (block-rotate-ccw c (block-rotate-ccw c (block-rotate-ccw c b))))

(provide
 block-rotate-ccw
 block-rotate-cw
 block=?
 block-move)
