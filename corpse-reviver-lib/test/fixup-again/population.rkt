#lang typed/racket

(provide population-payoffs)

(: population-payoffs (-> Population Boolean))

(require corpse-reviver/require-typed-check
         "automata-adapted.rkt")

(define (population-payoffs population0)
  #t)
