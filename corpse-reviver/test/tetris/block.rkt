#lang typed/racket

(require "base-types.rkt")
(require scv-cr/require-typed-check)
(require/typed/check "data.rkt"
                     [posn=? (-> Posn Posn Boolean)])

;; Determines if two blocks are the same (ignoring color).
(: block=? (-> Block Block Boolean))
(define (block=? b1 b2)
  (and (= (block-x b1) (block-x b2))
       (= (block-y b1) (block-y b2))))

(: block-move (-> Real Real Block Block))
(define (block-move dx dy b)
  (block (+ dx (block-x b))
         (+ dy (block-y b))
         (block-color b)))

;; Rotate the block 90 counterclockwise around the posn.
(: block-rotate-ccw (-> Posn Block Block))
(define (block-rotate-ccw c b)
  (block (+ (posn-x c) (- (posn-y c) (block-y b)))
         (+ (posn-y c) (- (block-x b) (posn-x c)))
         (block-color b)))

;; Rotate the block 90 clockwise around the posn.
(: block-rotate-cw (-> Posn Block Block))
(define (block-rotate-cw c b)
  (block-rotate-ccw c (block-rotate-ccw c (block-rotate-ccw c b))))

(provide
 block-rotate-ccw
 block-rotate-cw
 block=?
 block-move)
