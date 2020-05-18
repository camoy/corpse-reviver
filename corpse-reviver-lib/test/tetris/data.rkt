#lang typed/racket

(struct: posn ([x : Real]
               [y : Real]))
(struct: block ([x : Real]
                [y : Real]
                [color : Symbol]))
(struct: tetra ([center : posn]
                [blocks : (Listof block)]))
(struct: world ([tetra : tetra]
                [blocks : (Listof block)]))

(: posn=? (-> posn posn Boolean))
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(provide
 (struct-out posn)
 (struct-out block)
 (struct-out tetra)
 (struct-out world)
 posn=?)
