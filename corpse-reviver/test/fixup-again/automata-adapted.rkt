#lang typed/racket/base

(define-type Population (cons Automaton* Automaton*))
(define-type Automaton* [Vectorof Automaton])

(require "benchmark-util.rkt")
(require/typed/check "automata.rkt"
 [#:opaque Automaton automaton?])

(provide Population)
