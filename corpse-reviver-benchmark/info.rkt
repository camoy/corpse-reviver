#lang info

;; General

(define name "corpse-reviver-benchmark")
(define collection "corpse-reviver")
(define version "0.0")
(define raco-commands
  '(("scv-cr-benchmark" (submod corpse-reviver/benchmark main)
                        "Benchmark the performance of SCV-CR optimized programs."
                        #f)))
(define compile-omit-paths '("benchmarks"))

;; Dependencies

(define deps
  '("base"
    "corpse-reviver-lib"
    "gtp-measure"
    "gtp-util"
    "make-log-interceptor"
    "mischief"
    "rackunit"
    "threading"
    "git://github.com/racket/typed-racket"
    #;"typed-racket-lib"
    #;"typed-racket-more"))

(define build-deps '())
