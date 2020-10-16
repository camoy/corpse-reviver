#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in rackunit require/expose)
         data/queue
         gtp-measure/private/parse
         racket/file
         racket/place
         racket/runtime-path
         racket/cmdline
         racket/list
         racket/system
         racket/path
         threading
         "private/config.rkt"
         "private/task.rkt")

(require/expose gtp-measure/private/configure (gtp-measure-data-dir))

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
   #f  ; baseline?
   #f  ; resume?
   10  ; iterations
   10  ; cutoff
   #f  ; no-skip?
   #f  ; typed-blame?
   10  ; num-samples
   10  ; sample-factor
   1   ; worker-count
   #f  ; gc-log?
   "." ; output-dir
   0   ; sample
   #f  ; benchmark
   #f  ; analyses
   #f  ; runtimes
   0   ; cfg-id
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
  (benchmark/scv-cr normalized-args))

;; [Vector String] → List
;; Converts command line arguments into arguments suitable for a call to
;; benchmark/scv-cr.
(define (parse argv)
  (command-line
   #:program "scv-cr-benchmark"
   #:argv argv
   #:once-each
   [("-b" "--baseline")
    "Don't optimize with SCV-CR"
    (set-config-baseline?! cfg #t)]

   [("-c" "--cutoff")
    cutoff
    "Maximum number of components to measure exhaustively"
    (set-config-cutoff! cfg (string->number cutoff))]

   [("-g" "--gc-log")
    "Log GC statistics during analysis"
    (set-config-gc-log?! cfg #t)]

   [("-i" "--iterations")
    iters
    "Number of iterations"
    (set-config-iterations! cfg (string->number iters))]

   [("-n" "--no-skip")
    "Don't skip analysis of modules prefixed with _"
    (set-config-no-skip?! cfg #t)]

   [("-t" "--typed-blame")
    "Keep blame information for typed modules"
    (set-config-typed-blame?! cfg #t)]

   [("-o" "--output")
    output-dir
    "Result output directory"
    (set-config-output-dir! cfg output-dir)]

   [("-r" "--resume")
    "Run an existing setup"
    (set-config-resume?! cfg #t)]

   [("-w" "--worker-count")
    worker-count
    "Number of parallel workers"
    (set-config-worker-count! cfg (string->number worker-count))]

   [("-R" "--num-samples")
    num-samples
    "Number of samples"
    (set-config-num-samples! cfg (string->number num-samples))]

   [("-S" "--sample-factor")
    sample-factor
    "Sample factor for calculating the sample size"
    (set-config-sample-factor! cfg (string->number sample-factor))]

   #:args targets
   targets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; benchmark/scv-cr

;; [Listof Path-String] → Any
;; Benchmarks the given GTP typed/untyped targets.
(define/contract (benchmark/scv-cr targets)
  (-> (listof valid-typed-untyped-target?) any)
  (output-specs!)

  ;; setup queue
  (define queue-semaphore (make-semaphore 1))
  (define target-queue (make-queue))
  (define target-seq
    (if (config-resume? cfg)
        (let ([n (length (directory-list (gtp-measure-data-dir)))])
          (in-range 1 (add1 n)))
        (in-list targets)))
  (for ([target target-seq])
    (enqueue! target-queue target))

  ;; delete stale results
  (unless (config-resume? cfg)
    (delete-directory/files (gtp-measure-data-dir) #:must-exist? #f))

  ;; run consumer threads
  (define thds
    (for/list ([_ (in-range (config-worker-count cfg))])
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
    (thread-wait thd)))

;; → Any
;; Outputs machine specification information (Unix only). Specifically data about
;; CPU and memory. As well as the Racket version number.
(define (output-specs!)
  (define timestamp (iso-timestamp))
  (define spec-filename (format "~a_specs.txt" timestamp))
  (define version-filename (format "~a_version.txt" timestamp))
  (with-output-to-file (build-path (config-output-dir cfg) spec-filename)
    (λ ()
      (system "cat /proc/cpuinfo /proc/meminfo")))
  (with-output-to-file (build-path (config-output-dir cfg) version-filename)
    (λ ()
      (displayln (version)))))
