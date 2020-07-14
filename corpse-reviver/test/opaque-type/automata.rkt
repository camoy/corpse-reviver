#lang racket

;; An N-states, N-inputs Automaton

(provide automaton?)

(struct automaton (current
                   original
                   payoff
                   table)
                   #:transparent)
