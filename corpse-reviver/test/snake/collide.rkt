#lang typed/racket

(require corpse-reviver/require-typed-check
         "data-adaptor.rkt")
(require/typed "const.rkt"
                     [BOARD-WIDTH Integer]
                     [BOARD-HEIGHT Integer])
(require/typed "data.rkt"
                     [posn=? (Posn Posn . -> . Boolean)])

;; Is the snake colliding with any of the walls?
(: snake-wall-collide? : (Snake . -> . Boolean))
(define (snake-wall-collide? snk)
  (head-collide? (car (snake-segs snk))))

(: head-collide? : (Posn . -> . Boolean))
(define (head-collide? p)
  (or (<= (posn-x p) 0)
      (>= (posn-x p) BOARD-WIDTH)
      (<= (posn-y p) 0)
      (>= (posn-y p) BOARD-HEIGHT)))

(: snake-self-collide? : (Snake . -> . Boolean))
(define (snake-self-collide? snk)
  (segs-self-collide? (car (snake-segs snk))
                      (cdr (snake-segs snk))))

(: segs-self-collide? : (Posn (Listof Posn) . -> . Boolean))
(define (segs-self-collide? h segs)
  (cond [(empty? segs) #f]
        [else (or (posn=? (car segs) h)
                  (segs-self-collide? h (cdr segs)))]))
(provide
 snake-wall-collide?
 snake-self-collide?)
