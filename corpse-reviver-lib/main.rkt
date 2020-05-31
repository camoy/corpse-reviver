#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [compile-files/scv-cr
   (->* () #:rest (listof path-string?) any)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require compiler/cm
         racket/cmdline
         racket/contract
         soft-contract/main
         threading
         "private/compile.rkt"
         "private/data.rkt"
         "private/elaborate.rkt"
         "private/logging.rkt"
         "private/optimize.rkt"
         "private/syntax.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(module+ main
  (define args (parse (current-command-line-arguments)))
  (apply compile-files/scv-cr args))

;; [Vector String] → List
;; Converts command line arguments into arguments suitable for a call to
;; compile-files/scv-cr.
(define (parse argv)
  (command-line
   #:program "scv-cr"
   #:argv argv
   #:args targets
   targets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; Path-String ... → Any
;; Compiles files at the given paths with SCV-CR.
(define (compile-files/scv-cr . -targets)
  (measure 'total
    (define targets (map canonicalize-path -targets))
    (for-each managed-compile-zo targets)
    (define mods (sort-by-dep (map make-mod targets)))
    (define opt-mods (optimize mods))
    (compile-modules opt-mods)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require racket/path
           racket/string
           rackunit)

  ;; Path-String
  ;; Directory where the tests are located.
  (define TEST-DIR "test")

  ;; Module-Path Module-Path → Void
  ;; Checks to make sure running optimization on the modules does not yield
  ;; an issue by running the main target.
  (define (test-optimize root main targets)
    (define at-root (λ~>> (build-path root) simplify-path path->complete-path))
    (define main* (at-root main))
    (apply compile-files/scv-cr (map at-root targets))
    (check-not-exn
     (λ ()
      (in-dir main*
        (parameterize ([current-namespace (make-base-namespace)])
          (dynamic-require main* #f))))))

  ;; String {[List-of String]} → Void
  ;; Takes benchmark name and runs all files with test-optimize.
  (define (test-benchmark benchmark)
    (let* ([benchmark-files (directory-list benchmark #:build? benchmark)]
           [benchmark-files* (filter benchmark-relevant? benchmark-files)]
           [main (findf is-main? benchmark-files*)])
      (test-case
        benchmark
        (test-optimize "." main benchmark-files*))))

  ;; [List-of String] Path → Boolean
  ;; Returns if this file is relevant to the benchmark. To be relevant, a path
  ;; must be a Racket file and not be a "fake" file or in the omission list.
  (define (benchmark-relevant? path)
    (define name (file-name-string path))
    (and (path-has-extension? path #".rkt")
         (not (string-prefix? name "_"))))

  ;; Path → String
  ;; Returns the file name of a path as a string.
  (define file-name-string
    (λ~>> file-name-from-path path->string))

  ;; Path → Boolean
  ;; Returns if this file is the main.
  (define is-main?
    (λ~>> file-name-string (string=? "main.rkt")))

  (parameterize ([current-directory TEST-DIR])
    (test-case
      "Adapter imports reprovides a module as opaque (with an opaque type)."
      (test-optimize "double-opaque"
                     "main.rkt"
                     '("main.rkt"
                       "adapter.rkt")))
    (test-case
      "Testing the identifier fixup based on FSM."
      (test-optimize "fixup-again"
                     "population.rkt"
                     '("population.rkt"
                       "automata-adapted.rkt"
                       "automata.rkt")))
    (test-case
      "Support for opaque types (from FSM)."
      (test-optimize "opaque-type"
                     "automata-adapted.rkt"
                     '("automata-adapted.rkt"
                       "automata.rkt")))
    (test-case
      "Subset of struct definitions from kCFA benchmark."
      (test-optimize "kcfa-data"
                     "ai.rkt"
                     '("ai.rkt"
                       "structs-adapted.rkt"
                       "structs.rkt")))
    (test-case
      "kCFA with potential for conflicting contract identifiers."
      (test-optimize "kcfa-conflict"
                     "main.rkt"
                     '("ai.rkt"
                       "benv-adapted.rkt"
                       "benv.rkt"
                       "denotable-adapted.rkt"
                       "denotable.rkt"
                       "main.rkt"
                       "structs-adapted.rkt"
                       "structs.rkt"
                       "time-adapted.rkt"
                       "time.rkt"
                       "ui.rkt")))
    (test-case
      "Adapter and module from LNM benchmark."
      (test-optimize "lnm-data"
                     "lnm-plot.rkt"
                     '("lnm-plot.rkt"
                       "plot-adapted.rkt"
                       "plot.rkt")))
    (test-case
      "LNM with only require/typed/provide."
      (test-optimize "lnm-require-typed-provide"
                     "lnm-plot.rkt"
                     '("lnm-plot.rkt"
                       "plot-adapted.rkt")))
    (test-case
      "Define and make-predicate."
      (test-optimize "predicate"
                     "main.rkt"
                     '("main.rkt"
                       "server.rkt")))
    (test-case
      "Struct predicate reference needs to be fixed."
      (test-optimize "redefine-struct-predicate"
                     "lnm-plot.rkt"
                     '("lnm-plot.rkt"
                       "plot-adapted.rkt"
                       "plot.rkt")))
    (test-case
      "Mutable struct exports."
      (test-optimize "mutable-struct"
                     "typed-data.rkt"
                     '("typed-data.rkt"
                       "data.rkt")))
    (test-case
      "Bad (doesn't fully verify)."
      (test-optimize "bad"
                     "main.rkt"
                     '("main.rkt"
                       "server.rkt")))
    (test-case
      "Good struct (fully verifies)."
      (test-optimize "good-struct"
                     "main.rkt"
                     '("main.rkt"
                       "server.rkt")))
    (test-case
      "Good struct (fully verifies)."
      (test-optimize "good-struct"
                     "main.rkt"
                     '("main.rkt"
                       "server.rkt")))
    (test-case
      "Bad struct (doesn't fully verify)."
      (test-optimize "bad-struct"
                     "main.rkt"
                     '("main.rkt"
                       "server.rkt")))
    (test-case
      "Test require/opaque."
      (test-optimize "require-opaque"
                     "main.rkt"
                     '("main.rkt")))
    (test-case
      "Test require/opaque (untyped)."
      (test-optimize "require-opaque-untyped"
                     "main.rkt"
                     '("main.rkt")))
    (test-case
      "Test require/typed/opaque."
      (test-optimize "require-typed-opaque"
                     "main.rkt"
                     '("main.rkt")))
    (test-case
      "Test require/typed/provide/opaque."
      (test-optimize "require-typed-provide-opaque"
                     "main.rkt"
                     '("main.rkt"
                       "adapter.rkt")))
    (test-case
      "Test rest argument."
      (test-optimize "rest-args"
                     "main.rkt"
                     '("main.rkt")))
    (test-benchmark "sieve")
    (test-benchmark "fsm")
    (test-benchmark "morsecode")
    (test-benchmark "zombie")
    (test-benchmark "zordoz")
    (test-benchmark "lnm")
    (test-benchmark "suffixtree")
    (test-benchmark "kcfa")
    (test-benchmark "snake")
    (test-benchmark "tetris")
    (test-benchmark "synth")
    (test-benchmark "gregor")
    ))
