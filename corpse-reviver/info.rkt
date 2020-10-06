#lang info

;; General

(define collection "corpse-reviver")
(define version "0.0")
(define raco-commands
  '(("scv-cr" (submod corpse-reviver main)
              "Optimize a Typed Racket program with SCV."
              #f)))
(define scribblings '(("scribblings/corpse-reviver.scrbl" ())))
(define compile-omit-paths '("test"))

;; Dependencies

(define deps
  '("base"
    "graph"
    "lang-file"
    "mischief"
    "git://github.com/philnguyen/soft-contract.git"
    "threading"
    "typed-racket-lib"
    "typed-racket-more"))

(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "scribble-lib"
    "chk"))
