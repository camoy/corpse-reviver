#lang info

(define scribblings
  '(("scribblings/corpse-reviver-benchmark.scrbl" ())))

(define raco-commands
  '(("scv-cr-benchmark" (submod corpse-reviver-benchmark main)
                        "Benchmark the performance of SCV-CR optimized programs."
                        #f)))
