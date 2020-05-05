#lang typed/racket

(require scv-cr/require-typed-check)

(require/typed/check "data.rkt"
  [#:struct posn ([x : Real]
                  [y : Real])]
  [#:struct snake ([dir : Dir]
                   [segs : (NEListof Posn)])]
  [#:struct world ([snake : Snake]
                   [food  : Posn])])

(define-type (NEListof A) (Pairof A (Listof A)))
(define-type Dir (U "up" "down" "left" "right"))
(define-type Snake snake)
(define-type World world)
(define-type Posn  posn)

(provide
 (struct-out posn)
 (struct-out snake)
 (struct-out world)
 Dir
 Snake
 World
 Posn
 NEListof)
