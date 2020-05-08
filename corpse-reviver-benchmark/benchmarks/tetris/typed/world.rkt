#lang typed/racket

(require "base-types.rkt")
(require corpse-reviver/require-typed-check)
(require/typed/check "bset.rkt"
   [blocks-union (-> BSet BSet BSet)]
   [blocks-max-x (-> BSet Real)]
   [blocks-min-x (-> BSet Real)]
   [blocks-max-y (-> BSet Real)])
(require/typed/check "tetras.rkt"
  [tetra-move (-> Real Real Tetra Tetra)]
  [tetra-rotate-ccw (-> Tetra Tetra)]
  [tetra-rotate-cw (-> Tetra Tetra)]
  [tetra-overlaps-blocks? (-> Tetra BSet Boolean)]
  [tetra-change-color (-> Tetra Color Tetra)])
(require/typed/check "aux.rkt"
  [list-pick-random (-> (Listof Tetra) Tetra)]
  [neg-1  Negative-Fixnum]
  [tetras (Listof Tetra)])
(require/typed/check "elim.rkt"
  [eliminate-full-rows (-> BSet BSet)])
(require/typed/check "consts.rkt"
  [board-height Integer]
  [board-width Integer])

(provide world-key-move
         next-world
         ghost-blocks)

;; Add the current tetra's blocks onto the world's block list,
;; and create a new tetra.
(: touchdown (-> World World))
(define (touchdown w)
  (world (list-pick-random tetras)
         (eliminate-full-rows (blocks-union (tetra-blocks (world-tetra w))
                                            (world-blocks w)))))

;; Take the current tetra and move it down until it lands.
(: world-jump-down (-> World World))
(define (world-jump-down w)
  (cond [(landed? w) w]
        [else (world-jump-down (world (tetra-move 0 1 (world-tetra w))
                                      (world-blocks w)))]))

;; Has the current tetra landed on blocks?
;; I.e., if we move the tetra down 1, will it touch any existing blocks?
(: landed-on-blocks? (-> World Boolean))
(define (landed-on-blocks? w)
  (tetra-overlaps-blocks? (tetra-move 0 1 (world-tetra w))
                          (world-blocks w)))

;; Has the current tetra landed on the floor?
(: landed-on-floor? (-> World Boolean))
(define (landed-on-floor? w)
  (= (blocks-max-y (tetra-blocks (world-tetra w)))
     (sub1 board-height)))

;; Has the current tetra landed?
(: landed? (-> World Boolean))
(define (landed? w)
  (or (landed-on-blocks? w)
      (landed-on-floor? w)))

;; Step the world, either touchdown or move the tetra down on step.
(: next-world (-> World World))
(define (next-world w)
  (cond [(landed? w) (touchdown w)]
        [else (world (tetra-move 0 1 (world-tetra w))
                     (world-blocks w))]))

;; Make a world with the new tetra *IF* if doesn't lie on top of some other
;; block or lie off the board. Otherwise, no change.
(: try-new-tetra (-> World Tetra World))
(define (try-new-tetra w new-tetra)
  (cond [(or (<  (blocks-min-x (tetra-blocks new-tetra)) 0)
             (>= (blocks-max-x (tetra-blocks new-tetra)) board-width)
             (tetra-overlaps-blocks? new-tetra (world-blocks w)))
         w]
        [else (world new-tetra (world-blocks w))]))

;; Move the Tetra by the given X & Y displacement, but only if you can.
;; Otherwise stay put.
(: world-move (-> Real Real World World))
(define (world-move dx dy w)
  (try-new-tetra w (tetra-move dx dy (world-tetra w))))


;; Rotate the Tetra 90 degrees counterclockwise, but only if you can.
;; Otherwise stay put.
(: world-rotate-ccw (-> World World))
(define (world-rotate-ccw w)
  (try-new-tetra w (tetra-rotate-ccw (world-tetra w))))

;; Rotate the Tetra 90 degrees clockwise, but only if you can.
;; Otherwise stay put.
(: world-rotate-cw (-> World World))
(define (world-rotate-cw w)
  (try-new-tetra w (tetra-rotate-cw (world-tetra w))))

;; Gray blocks representing where the current tetra would land.
(: ghost-blocks (-> World BSet))
(define (ghost-blocks w)
  (tetra-blocks (tetra-change-color (world-tetra (world-jump-down w))
                                    'gray)))

;; Move the world according to the given key event.
(: world-key-move (-> World String World))
(define (world-key-move w k)
  (cond [(equal? k "left") (world-move neg-1 0 w)]
        [(equal? k "right") (world-move 1 0 w)]
        [(equal? k "down") (world-jump-down w)]
        [(equal? k "a") (world-rotate-ccw w)]
        [(equal? k "s") (world-rotate-cw w)]
        [else w]))

