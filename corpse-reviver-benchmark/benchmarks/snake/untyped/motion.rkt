#lang racket
(require "data.rkt"
         "const.rkt"
         "motion-help.rkt")

(provide reset!)
(define (reset!)
  (random-seed 1324))

;; world->world : World -> World
(define (world->world w)
  (cond [(eating? w) (snake-eat w)]
        [else
         (world (snake-slither (world-snake w))
                (world-food w))]))
;; eating? : World -> Boolean
;; Is the snake eating the food in the world.
(define (eating? w)
  (posn=? (world-food w)
          (car (snake-segs (world-snake w)))))
;; snake-change-direction : Snake Direction -> Snake
;; Change the direction of the snake.
(define (snake-change-direction snk dir)
  (snake dir
         (snake-segs snk)))
;; world-change-dir : World Direction -> World
;; Change direction of the world.
(define (world-change-dir w dir)
  (world (snake-change-direction (world-snake w) dir)
         (world-food w)))
;; snake-eat : World -> World
;; Eat the food and generate a new one.
(define (snake-eat w)
  (define i (add1 (random (sub1 BOARD-WIDTH))))
  (define j (add1 (random (sub1 BOARD-HEIGHT))))
  (world (snake-grow (world-snake w))
         (posn i j)

         #;(posn (- BOARD-WIDTH 1) (- BOARD-HEIGHT 1))))
(provide
 world-change-dir
 world->world)
