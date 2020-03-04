#lang info

;; Scribble

(define scribblings
  '(("scribblings/corpse-reviver-benchmark.scrbl" ())))

;; Raco

(define raco-commands
  '(("scv-cr-benchmark" (submod corpse-reviver-benchmark main)
                        "Benchmark the performance of SCV-CR optimized programs."
                        #f)))
