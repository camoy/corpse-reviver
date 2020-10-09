#lang info

;; General

(define collection "corpse-reviver-artifact")
(define version "0.0")
(define scribblings
  '(("scribblings/corpse-reviver-artifact.scrbl" (always-run no-search))))

(define deps '("git://github.com/camoy/gtp-plot.git#corpse-reviver"
               "gtp-util"
               "gui-lib"
               "math-lib"
               "mischief"
               "pict-abbrevs"
               "pict-lib"
               "ppict"
               "scribble-lib"
               "threading-lib"
               "base"))
(define build-deps '("chk-lib"
                     "corpse-reviver"
                     "corpse-reviver-benchmark"
                     "racket-doc"
                     "rackunit-lib"
                     "typed-racket-doc"
                     "typed-racket-lib"
                     ))
