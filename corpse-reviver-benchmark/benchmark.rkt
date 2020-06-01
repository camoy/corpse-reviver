#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [benchmark/scv-cr (-> any)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require data/queue
         gtp-measure/private/parse
         racket/place
         racket/runtime-path
         racket/cmdline
         racket/list
         racket/system
         racket/path
         threading
         "private/task.rkt"
         "private/util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; consts

;; [Listof String]
;; The names of the benchmarks that are run without command-line files. These
;; should all be present in this directory's `benchmarks/`.
(define DEFAULT-BENCHMARKS
  '("sieve"
    "fsm"
    "morsecode"
    "zombie"
    "zordoz"
    "lnm"
    "suffixtree"
    "kcfa"
    "snake"
    "tetris"
    "synth"
    "gregor"))

;; Path
;; Useful local paths.
(define-runtime-path BENCHMARK-DIR "benchmarks")
(define-runtime-path TASK "private/task.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config

(define cfg
  (config
   10  ; iterations
   10  ; cutoff
   #f  ; no-skip?
   4   ; num-samples
   10  ; sample-factor
   "." ; output-dir
   0   ; sample
   #f  ; benchmark
   #f  ; analyses
   #f  ; runtimes
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(module+ main
  (define -args (parse (current-command-line-arguments)))
  (define args
    (if (empty? -args)
        (map (λ~>> (build-path BENCHMARK-DIR)) DEFAULT-BENCHMARKS)
        -args))
  (define normalized-args (map (λ~> normalize-path path->string) args))
  (apply benchmark/scv-cr normalized-args))

;; [Vector String] → List
;; Converts command line arguments into arguments suitable for a call to
;; benchmark/scv-cr.
(define (parse argv)
  (command-line
   #:program "scv-cr-benchmark"
   #:argv argv
   #:once-each
   [("-i" "--iterations")
    iters
    "Number of iterations"
    (set-config-iterations! cfg (string->number iters))]

   [("-c" "--cutoff")
    cutoff
    "Maximum number of components to measure exhaustively"
    (set-config-cutoff! cfg (string->number cutoff))]

   [("-n" "--no-skip")
    "Don't skip analysis of modules prefixed with _"
    (set-config-no-skip?! cfg #t)]

   [("-S" "--sample-factor")
    sample-factor
    "Sample factor for calculating the sample size"
    (set-config-sample-factor! cfg (string->number sample-factor))]

   [("-R" "--num-samples")
    num-samples
    "Number of samples"
    (set-config-num-samples! cfg (string->number num-samples))]

   [("-o" "--output")
    output-dir
    "Result output directory"
    (set-config-output-dir! cfg output-dir)]

   #:args targets
   targets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; benchmark/scv-cr

;; Path-String ... → Any
;; Benchmarks the given GTP typed/untyped targets.
(define/contract (benchmark/scv-cr . targets)
  (->* () #:rest (listof valid-typed-untyped-target?) any)
  (output-specs!)

  ;; setup queue
  (define queue-semaphore (make-semaphore 1))
  (define target-queue (make-queue))
  (for ([target (in-list targets)])
    (enqueue! target-queue target))

  ;; run consumer threads
  (define thds
    (for/list ([_ (in-range (processor-count))])
      (thread
       (λ ()
         (let go ()
           (define maybe-target
             (call-with-semaphore
              queue-semaphore
              (λ () (and (non-empty-queue? target-queue)
                         (dequeue! target-queue)))))
           (when maybe-target
             (define pl (dynamic-place TASK 'place-benchmark))
             (place-channel-put pl maybe-target)
             (place-channel-put pl (struct->vector cfg))
             (place-wait pl)
             (go)))))))
  (for ([thd (in-list thds)])
    (thread-wait thds)))

;; → Any
;; Outputs machine specification information (Unix only). Specifically data about
;; CPU and memory.
(define (output-specs!)
  (define filename (format "~a_specs.txt" (iso-timestamp)))
  (with-output-to-file (build-path (config-output-dir cfg) filename)
    (λ ()
      (system "cat /proc/cpuinfo /proc/meminfo"))))
