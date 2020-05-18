#lang typed/racket/base

(require "_benchmark-util.rkt")
(require/typed "automata.rkt"
 [#:opaque Automaton automaton?])

(provide Automaton)
