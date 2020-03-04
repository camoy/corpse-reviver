#lang info

;; Scribble

(define scribblings
  '(("scribblings/corpse-reviver.scrbl" ())))

;; Raco

(define raco-commands
  '(("scv-cr" (submod corpse-reviver main)
              "Optimize a Typed Racket program with SCV."
              #f)))
