#lang racket

(struct snake (dir segs))
(struct world (snake food))
(struct posn (x y))

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))  

(provide [struct-out posn])

(provide
 posn=?
 [struct-out snake]
 [struct-out world])
