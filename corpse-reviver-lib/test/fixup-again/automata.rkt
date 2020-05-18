#lang typed/racket

(define-type Automaton automaton)
(provide Automaton)
(struct automaton () #:transparent)
