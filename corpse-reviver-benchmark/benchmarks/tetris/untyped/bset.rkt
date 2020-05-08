#lang racket

(require "data.rkt"
         "block.rkt"
         "consts.rkt")

;; blocks-contains? : BSet Block -> Boolean
;; Determine if the block is in the set of blocks.
(define (blocks-contains? bs b)
  (ormap (λ (c) (block=? b c)) bs))

;; blocks-subset? : BSet BSet -> Boolean
;; is every element in bs1 also in bs2?
(define (blocks-subset? bs1 bs2)
  (andmap (λ (b) (blocks-contains? bs2 b)) bs1))

;; blocks=? : BSet BSet -> Boolean
;; Determine if given sets of blocks are equal.
(define (blocks=? bs1 bs2)
  (and (blocks-subset? bs1 bs2)
       (blocks-subset? bs2 bs1)))

;; blocks-intersect : BSet BSet -> BSet
;; Return the set of blocks that appear in both sets.
(define (blocks-intersect bs1 bs2)
  (filter (λ (b) (blocks-contains? bs2 b)) bs1))

;; blocks-count : BSet -> Nat
;; Return the number of blocks in the set.
(define (blocks-count bs)
  (length bs))  ;; No duplicates, cardinality = length.

;; blocks-move : Number Number BSet -> BSet
;; Move each block by the given X & Y displacement.
(define (blocks-move dx dy bs)
  (map (λ (b) (block-move dx dy b)) bs))

;; blocks-rotate-ccw : Posn BSet -> BSet
;; Rotate the blocks 90 counterclockwise around the posn.
(define (blocks-rotate-ccw c bs)
  (map (λ (b) (block-rotate-ccw c b)) bs))

;; blocks-rotate-cw : Posn BSet -> BSet
;; Rotate the blocks 90 clockwise around the posn.
(define (blocks-rotate-cw c bs)
  (map (λ (b) (block-rotate-cw c b)) bs))

;; blocks-change-color : BSet Color -> BSet
(define (blocks-change-color bs c)
  (map (λ (b) (block (block-x b) (block-y b) c))
       bs))

;; blocks-row : BSet Number -> BSet
;; Return the set of blocks in the given row.
(define (blocks-row bs i)
  (filter (λ (b) (= i (block-y b))) bs))

;; full-row? : BSet Nat -> Boolean
;; Are there a full row of blocks at the given row in the set.
(define (full-row? bs i)
  (= board-width (blocks-count (blocks-row bs i))))

;; blocks-overflow? : BSet -> Boolean
;; Have any of the blocks reach over the top of the board?
(define (blocks-overflow? bs)
  (ormap (λ (b) (<= (block-y b) 0)) bs))

;; blocks-union : BSet BSet -> BSet
;; Union the two sets of blocks.
(define (blocks-union bs1 bs2)
  (foldr (λ (b bs)
           (cond [(blocks-contains? bs b) bs]
                 [else (cons b bs)]))
         bs2
         bs1))

;; blocks-max-y : BSet -> Number
;; Compute the maximum y coordinate;
;; if set is empty, return 0, the coord of the board's top edge.
(define (blocks-max-y bs)
  (foldr (λ (b n) (max (block-y b) n)) 0 bs))

;; blocks-min-x : BSet -> Number
;; Compute the minimum x coordinate;
;; if set is empty, return the coord of the board's right edge.
(define (blocks-min-x bs)
  (foldr (λ (b n) (min (block-x b) n)) board-width bs))

;; blocks-max-x : BSet -> Number
;; Compute the maximum x coordinate;
;; if set is empty, return 0, the coord of the board's left edge.
(define (blocks-max-x bs)
  (foldr (λ (b n) (max (block-x b) n)) 0 bs))

(provide
 blocks-contains?
 blocks=?
 blocks-subset?
 blocks-intersect
 blocks-count
 blocks-overflow?
 blocks-move
 blocks-rotate-cw
 blocks-rotate-ccw
 blocks-change-color
 blocks-row
 full-row?
 blocks-union
 blocks-max-x
 blocks-min-x
 blocks-max-y)
