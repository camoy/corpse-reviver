#lang info

(define scribblings
  '(("scribblings/corpse-reviver.scrbl" ())))

(define raco-commands
  '(("scv-cr" (submod corpse-reviver main)
              "Optimize a Typed Racket program with SCV."
              #f)))

(define compile-omit-paths '("test"))
