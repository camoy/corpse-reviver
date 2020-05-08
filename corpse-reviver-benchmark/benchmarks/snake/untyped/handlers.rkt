#lang racket
;; Movie handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(require "data.rkt"
         "motion.rkt"
         "collide.rkt")

;; handle-key : World String -> World
(define (handle-key w ke)
  (cond [(equal? ke "w") (world-change-dir w "up")]
        [(equal? ke "s") (world-change-dir w "down")]
        [(equal? ke "a") (world-change-dir w "left")]
        [(equal? ke "d") (world-change-dir w "right")]
        [else w]))

;; game-over? : World -> Boolean
(define (game-over? w)
  (or (snake-wall-collide? (world-snake w))
      (snake-self-collide? (world-snake w))))

(provide
 handle-key 
 game-over?)
