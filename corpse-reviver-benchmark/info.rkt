#lang info

;; General

(define collection "corpse-reviver-benchmark")
(define version "0.0")
(define raco-commands
  '(("scv-cr-benchmark" (submod corpse-reviver-benchmark main)
                        "Benchmark the performance of SCV-CR optimized programs."
                        #f)))
(define scribblings '(("scribblings/corpse-reviver-benchmark.scrbl" ())))
(define compile-omit-paths '("benchmarks"))

;; Dependencies

(define deps
  '("base"
    "corpse-reviver"
    "gtp-measure"
    "gtp-util"
    "make-log-interceptor"
    "mischief"
    "rackunit"
    "threading"
    "typed-racket-lib"
    "typed-racket-more"))

(define build-deps
   '("gtp-benchmarks"
     "scribble-lib"))
