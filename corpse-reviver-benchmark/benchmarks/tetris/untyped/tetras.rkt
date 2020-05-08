#lang racket

(require "bset.rkt"
         "data.rkt"
         "consts.rkt"
         "block.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tetras

;; tetra-move : Number Number Tetra -> Tetra
;; Move the Tetra by the given X & Y displacement.
(define (tetra-move dx dy t)
  (tetra (posn (+ dx (posn-x (tetra-center t)))
               (+ dy (posn-y (tetra-center t))))
         (blocks-move dx dy (tetra-blocks t))))

;; tetra-rotate-ccw : Tetra -> Tetra
;; Rotate the tetra 90 degrees counterclockwise around its center.
(define (tetra-rotate-ccw t)
  (tetra (tetra-center t)
         (blocks-rotate-ccw (tetra-center t)
                            (tetra-blocks t))))

;; tetra-rotate-cw : Tetra -> Tetra
;; Rotate the tetra 90 degrees clockwise around its center.
(define (tetra-rotate-cw t)
  (tetra (tetra-center t)
         (blocks-rotate-cw (tetra-center t)
                           (tetra-blocks t))))

;; tetra-overlaps-blocks? : Tetra BSet -> Boolean
;; Is the tetra on any of the blocks?
(define (tetra-overlaps-blocks? t bs)
  (not (empty? (blocks-intersect (tetra-blocks t) bs))))

;; tetra-change-color : Tetra Color -> Tetra
;; Change the color of the given tetra.
(define (tetra-change-color t c)
  (tetra (tetra-center t)
         (blocks-change-color (tetra-blocks t) c)))

(define (build-tetra-blocks color xc yc x1 y1 x2 y2 x3 y3 x4 y4)
  (tetra-move 3 0 
              (tetra (posn xc yc)
                     (list (block x1 y1 color)
                           (block x2 y2 color)
                           (block x3 y3 color)
                           (block x4 y4 color)))))

(provide
 tetra-move
 tetra-rotate-ccw
 tetra-rotate-cw
 tetra-overlaps-blocks?
 build-tetra-blocks
 tetra-change-color)
