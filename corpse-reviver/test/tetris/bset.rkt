#lang typed/racket

(require "base-types.rkt")
(require scv-cr/require-typed-check)
(require/typed/check "block.rkt"
  [block-rotate-ccw (-> Posn Block Block)]
  [block-rotate-cw (-> Posn Block Block)]
  [block=? (-> Block Block Boolean)]
  [block-move (-> Real Real Block Block)])
(require/typed/check "consts.rkt"
  [board-width Integer])

;; Determine if the block is in the set of blocks.
(: blocks-contains? (-> BSet Block Boolean))
(define (blocks-contains? bs b)
  (ormap (λ: ([c : Block]) (block=? b c)) bs))

;; is every element in bs1 also in bs2?
(: blocks-subset? (-> BSet BSet Boolean))
(define (blocks-subset? bs1 bs2)
  (andmap (λ: ([b : Block]) (blocks-contains? bs2 b)) bs1))

;; Determine if given sets of blocks are equal.
(: blocks=? (-> BSet BSet Boolean))
(define (blocks=? bs1 bs2)
  (and (blocks-subset? bs1 bs2)
       (blocks-subset? bs2 bs1)))

;; Return the set of blocks that appear in both sets.
(: blocks-intersect (-> BSet BSet BSet))
(define (blocks-intersect bs1 bs2)
  (filter (λ: ([b : Block]) (blocks-contains? bs2 b)) bs1))

;; Return the number of blocks in the set.
(: blocks-count (-> BSet Natural))
(define (blocks-count bs)
  (length bs))  ;; No duplicates, cardinality = length.

;; Move each block by the given X & Y displacement.
(: blocks-move (-> Real Real BSet BSet))
(define (blocks-move dx dy bs)
  (map (λ: ([b : Block]) (block-move dx dy b)) bs))

;; Rotate the blocks 90 counterclockwise around the posn.
(: blocks-rotate-ccw (-> Posn BSet BSet))
(define (blocks-rotate-ccw c bs)
  (map (λ: ([b : Block]) (block-rotate-ccw c b)) bs))

;; Rotate the blocks 90 clockwise around the posn.
(: blocks-rotate-cw (-> Posn BSet BSet))
(define (blocks-rotate-cw c bs)
  (map (λ: ([b : Block]) (block-rotate-cw c b)) bs))

(: blocks-change-color (-> BSet Color BSet))
(define (blocks-change-color bs c)
  (map (λ: ([b : Block]) (block (block-x b) (block-y b) c))
       bs))

;; Return the set of blocks in the given row.
(: blocks-row (-> BSet Real BSet))
(define (blocks-row bs i)
  (filter (λ: ([b : Block]) (= i (block-y b))) bs))

;; Are there a full row of blocks at the given row in the set.
(: full-row? (-> BSet Natural Boolean))
(define (full-row? bs i)
  (= board-width (blocks-count (blocks-row bs i))))

;; Have any of the blocks reach over the top of the board?
(: blocks-overflow? (-> BSet Boolean))
(define (blocks-overflow? bs)
  (ormap (λ: ([b : Block]) (<= (block-y b) 0)) bs))

;; Union the two sets of blocks.
(: blocks-union (-> BSet BSet BSet))
(define (blocks-union bs1 bs2)
  (foldr (λ: ([b  : Block]
              [bs : BSet])
           (cond [(blocks-contains? bs b) bs]
                 [else (cons b bs)]))
         bs2
         bs1))

;; Compute the maximum y coordinate;
;; if set is empty, return 0, the coord of the board's top edge.
(: blocks-max-y (-> BSet Real))
(define (blocks-max-y bs)
  (foldr (λ: ([b : Block]
              [n : Real])
           (max (block-y b) n)) 0 bs))

;; Compute the minimum x coordinate;
;; if set is empty, return the coord of the board's right edge.
(: blocks-min-x (-> BSet Real))
(define (blocks-min-x bs)
  (foldr (λ: ([b : Block]
              [n : Real])
           (min (block-x b) n)) board-width bs))

;; Compute the maximum x coordinate;
;; if set is empty, return 0, the coord of the board's left edge.
(: blocks-max-x (-> BSet Real))
(define (blocks-max-x bs)
  (foldr (λ: ([b : Block] 
              [n : Real])
           (max (block-x b) n)) 0 bs))

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
