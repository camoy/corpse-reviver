#lang typed/racket/base

(provide posn-adder-x
         (struct-out posn))

(struct posn ([x : Real] [y : Real]))

(: posn-adder-x (-> posn posn))
(define (posn-adder-x p)
  (posn (+ (posn-x p) 1) (posn-y p)))
