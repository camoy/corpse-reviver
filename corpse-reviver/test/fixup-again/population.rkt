#lang typed/racket

(provide population-payoffs)

(: population-payoffs (-> Population Boolean))

(require scv-cr/require-typed-check
         "automata-adapted.rkt")

(define (population-payoffs population0)
  #t)
