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

(require racket/cmdline
         racket/contract
         soft-contract/main
         threading
         "private/compile.rkt"
         "private/data.rkt"
         "private/elaborate.rkt"
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
  (define targets (map canonicalize-path -targets))
  (for ([target (in-list targets)])
    (delete-bytecode target))

  (define mods (sort-by-dep (map make-mod targets)))
  (define opt-mods (optimize mods))
  (compile-modules opt-mods))
