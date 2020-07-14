#lang typed/racket
(require "data-adaptor.rkt")

(define GRID-SIZE 30)

(define BOARD-HEIGHT 20)

(define BOARD-WIDTH 30)

(define (BOARD-HEIGHT-PIXELS) (* GRID-SIZE BOARD-HEIGHT))

(define (BOARD-WIDTH-PIXELS) (* GRID-SIZE BOARD-WIDTH))

(define (SEGMENT-RADIUS) (/ GRID-SIZE 2))
(define (FOOD-RADIUS) (SEGMENT-RADIUS))
(define (WORLD) (world (snake "right" (cons (posn 5 3) empty))
                       (posn 8 12)))

(provide
 WORLD
 GRID-SIZE
 BOARD-HEIGHT-PIXELS
 BOARD-WIDTH
 BOARD-HEIGHT)
