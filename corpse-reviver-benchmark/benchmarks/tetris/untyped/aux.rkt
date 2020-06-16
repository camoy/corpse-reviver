#lang racket

(require "data.rkt"
         "tetras.rkt")  

(provide
 list-pick-random
 neg-1
 tetras)

(random-seed 43453)


(define (list-pick-random ls)
  (list-ref ls (random (length ls))))

(define neg-1 -1)

(define tetras
  (list 
   (build-tetra-blocks 'green   1/2 -3/2    0 -1 0 -2 1 -1 1 -2)
   (build-tetra-blocks 'blue    1   -1      0 -1 1 -1 2 -1 3 -1)
   (build-tetra-blocks 'purple  1   -1      0 -1 1 -1 2 -1 2 -2)
   (build-tetra-blocks 'cyan    1   -1      0 -1 1 -1 2 -1 0 -2)
   (build-tetra-blocks 'orange  1   -1      0 -1 1 -1 2 -1 1 -2)
   (build-tetra-blocks 'red     1   -1      0 -1 1 -1 1 -2 2 -2)
   (build-tetra-blocks 'pink    1   -1      0 -2 1 -2 1 -1 2 -1)))
