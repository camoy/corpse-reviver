#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out config)
         iso-timestamp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/date)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configuration

(struct config
  (baseline?
   iterations
   cutoff
   no-skip?
   num-samples
   sample-factor
   worker-count
   output-dir
   sample
   benchmark
   analyses
   runtimes)
  #:prefab
  #:mutable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; → String
;; Get the current time in ISO-8601 format.
(define (iso-timestamp)
  (parameterize ([date-display-format 'iso-8601])
    (date->string (current-date) #t)))
