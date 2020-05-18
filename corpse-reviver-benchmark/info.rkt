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
  '(("base" #:version "7.4")
    "corpse-reviver-lib"
    "csv-writing"
    "gtp-measure"
    "gtp-util"
    "make-log-interceptor"
    "mischief"
    "rackunit"
    "threading"
    ("typed-racket-lib" #:version "1.10")
    ("typed-racket-more" #:version "1.10")))

(define build-deps '())
