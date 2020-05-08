#lang racket

(require "data.rkt"
         "bset.rkt"
         "tetras.rkt"
         "aux.rkt"
         "elim.rkt"
         "consts.rkt")

(provide world-key-move
         next-world
         ghost-blocks)

;; touchdown : World -> World
;; Add the current tetra's blocks onto the world's block list,
;; and create a new tetra.
(define (touchdown w)
  (world (list-pick-random tetras)
         (eliminate-full-rows (blocks-union (tetra-blocks (world-tetra w))
                                            (world-blocks w)))))

;; world-jump-down : World -> World
;; Take the current tetra and move it down until it lands.
(define (world-jump-down w)
  (cond [(landed? w) w]
        [else (world-jump-down (world (tetra-move 0 1 (world-tetra w))
                                      (world-blocks w)))]))

;; landed-on-blocks? : World -> Boolean
;; Has the current tetra landed on blocks?
;; I.e., if we move the tetra down 1, will it touch any existing blocks?
(define (landed-on-blocks? w)
  (tetra-overlaps-blocks? (tetra-move 0 1 (world-tetra w))
                          (world-blocks w)))

;; landed-on-floor? : World -> Boolean
;; Has the current tetra landed on the floor?
(define (landed-on-floor? w)
  (= (blocks-max-y (tetra-blocks (world-tetra w)))
     (sub1 board-height)))

;; landed? : World -> Boolean
;; Has the current tetra landed?
(define (landed? w)
  (or (landed-on-blocks? w)
      (landed-on-floor? w)))

;; next-world : World -> World
;; Step the world, either touchdown or move the tetra down on step.
(define (next-world w)
  (cond [(landed? w) (touchdown w)]
        [else (world (tetra-move 0 1 (world-tetra w))
                     (world-blocks w))]))

;; try-new-tetra : World Tetra -> World
;; Make a world with the new tetra *IF* if doesn't lie on top of some other
;; block or lie off the board. Otherwise, no change.
(define (try-new-tetra w new-tetra)
  (cond [(or (<  (blocks-min-x (tetra-blocks new-tetra)) 0)
             (>= (blocks-max-x (tetra-blocks new-tetra)) board-width)
             (tetra-overlaps-blocks? new-tetra (world-blocks w)))
         w]
        [else (world new-tetra (world-blocks w))]))

;; world-move : Number Number World -> World
;; Move the Tetra by the given X & Y displacement, but only if you can.
;; Otherwise stay put.
(define (world-move dx dy w)
  (try-new-tetra w (tetra-move dx dy (world-tetra w))))

;; world-rotate-ccw : World -> World
;; Rotate the Tetra 90 degrees counterclockwise, but only if you can.
;; Otherwise stay put.
(define (world-rotate-ccw w)
  (try-new-tetra w (tetra-rotate-ccw (world-tetra w))))

;; world-rotate-cw : World -> World
;; Rotate the Tetra 90 degrees clockwise, but only if you can.
;; Otherwise stay put.
(define (world-rotate-cw w)
  (try-new-tetra w (tetra-rotate-cw (world-tetra w))))

;; ghost-blocks : World -> BSet
;; Gray blocks representing where the current tetra would land.
(define (ghost-blocks w)
  (tetra-blocks (tetra-change-color (world-tetra (world-jump-down w))
                                    'gray)))

;; world-key-move : World KeyEvent -> World
;; Move the world according to the given key event.
(define (world-key-move w k)
  (cond [(equal? k "left") (world-move neg-1 0 w)]
        [(equal? k "right") (world-move 1 0 w)]
        [(equal? k "down") (world-jump-down w)]
        [(equal? k "a") (world-rotate-ccw w)]
        [(equal? k "s") (world-rotate-cw w)]
        [else w]))

