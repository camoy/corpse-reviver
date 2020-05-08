#lang racket

(struct posn (x y))
(struct block (x y color))
(struct tetra (center blocks))
(struct world (tetra blocks))

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(provide
 (struct-out block)
 (struct-out posn)
 (struct-out tetra)
 (struct-out world)
 posn=?)
#;
(provide
 (contract-out
  (struct block ([x real?] [y real?] [color COLOR/C]))
  (struct posn ([x real?] [y real?]))
  (struct tetra ([center POSN/C] [blocks BSET/C]))
  (struct world ([tetra TETRA/C] [blocks BSET/C]))       
  [posn=? (POSN/C POSN/C . -> . boolean?)])
 COLOR/C
 POSN/C
 BLOCK/C
 TETRA/C
 WORLD/C
 BSET/C)
