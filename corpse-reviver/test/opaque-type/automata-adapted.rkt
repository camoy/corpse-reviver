#lang typed/racket/base

(require "benchmark-util.rkt")
(require/typed "automata.rkt"
 [#:opaque Automaton automaton?])

(provide Automaton)
