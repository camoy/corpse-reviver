#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in corpse-reviver/private/logging scv-cr-logger)
         (only-in rackunit require/expose)
         corpse-reviver
         csv-writing
         data/queue
         fancy-app
         gtp-measure/private/check-pkg-deps
         gtp-measure/private/configure
         gtp-measure/private/parse
         gtp-measure/private/task
         gtp-util
         gcstats/core
         mischief/for
         racket/cmdline
         racket/list
         racket/match
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         threading
         "private/logging.rkt")

(require/expose gtp-measure/private/task (gtp-measure-subtask-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define BENCHMARKS
  '("sieve"
    #;"fsm"
    #;"morsecode"
    #;"zombie"
    #;"zordoz"
    #;"lnm"
    #;"suffixtree"
    #;"kcfa"
    #;"snake"
    #;"tetris"
    #;"synth"
    #;"gregor"))
(define THIS-SYM 'scv-cr-benchmark)
(define THIS-STR "scv-cr-benchmark")
(define-runtime-path CWD ".")
(define BENCHMARK-DIR (build-path CWD "benchmarks"))
(define current-target (make-parameter #f))
(define current-timing (make-parameter #f))
(define current-config-timing (make-parameter #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(module+ main
  (define config (parse (current-command-line-arguments)))
  (define targets (map (build-path BENCHMARK-DIR _) BENCHMARKS))
  (define-values (task timings) (measure-task targets config))
  (display-table (measure-data->table (measure-data task timings))))

;; [Vector String] → List
;; Converts command line arguments into arguments suitable for a call to
;; compile-files/scv-cr.
(define (parse argv)
  (define config
    (make-hash (list (cons key:bin (path->string (build-path CWD "bin"))))))
  (command-line
   #:program "scv-cr-benchmark"
   #:argv argv
   #:once-any
   [("-i" "--iterations")
    iters
    "Number of iterations"
    (hash-set! config key:iterations (read/ctc iters exact-positive-integer?))]

   [("-c" "--cutoff")
    N
    "Max. number of components to measure exhaustively (vs. by sampling)"
    (hash-set! config key:cutoff (read/ctc N exact-nonnegative-integer?))]

   [("-S" "--sample-factor")
    sf
    "Determines sample size (sample-size = S * num-components)"
    (hash-set! config key:sample-factor (read/ctc sf exact-nonnegative-integer?))]

   [("-R" "--num-samples")
    ns
    "Number of samples"
    (hash-set! config key:num-samples (read/ctc ns exact-positive-integer?))]

   [("--max-config-time")
    time-limit
    "time limit to run all iterations for one config. (h, m, or s)"
    (hash-set! config key:time-limit (string->time-limit time-limit))]

   [("--warmup")
    iters
    "JIT warmup iterations"
    (hash-set! config key:jit-warmup (read/ctc iters exact-nonnegative-integer?))]
   #:args ()
   config))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main functions

;; [Listof Path-String] Config → Task [Hash Path-String Timing]
(define (measure-task targets config)
  (define targets*
    (for/list ([tgt (in-list targets)])
      (cons (path->string (normalize-path tgt))
            (valid-target? tgt))))
  (define config* (init-config (hash->immutable-hash config)))
  (define task (init-task targets* config*))
  (for ([cfg (in-list (task->config* task))])
    (check-pkg-deps (config-ref cfg key:bin) #:auto? #true))
  (define timings
    (for/hash ([tgt targets]
               [st (in-list (in-subtasks task))])
      (define timing (make-queue))
      (parameterize ([current-file-runner file-runner]
                     [current-target tgt]
                     [current-timing timing])
        (subtask-run! st))
      (values (gtp-measure-subtask-out st) (queue->list timing))))
  (values task timings))

;; Path-String Config → Any
(define (file-runner main config)
  (define main-dir (path-only (path->complete-path (string->path main))))
  (define targets
    (filter (λ (x) (and (rkt? x) (not (fake-prefixed? x))))
            (directory-list main-dir #:build? main-dir)))
  (λ ([out-port (current-output-port)])
    (define config-timing (make-queue))

    ;; compile
    (with-intercepted-logging
      (λ (evt)
        (define logger (vector-ref evt 3))
        (define datum
          (~> (vector-ref evt 1)
              (substring (string-length (format "~a: " logger)))
              open-input-string
              read))
        (enqueue! config-timing datum))
      (λ ()
        (define initial-times
          (cons (current-process-milliseconds) (current-inexact-milliseconds)))
        (define buf (box '()))
        (define reciever (make-log-receiver (current-logger) 'debug))
        (define (handler)
          (letrec-values
              ([(L) (λ ()
                      (define v (sync reciever))
                      (if (eq? 'gc-info (prefab-struct-key (vector-ref v 2)))
                          (set-box! buf (cons v (unbox buf)))
                          (void))
                      (L))])
            (L)))
        (define thr (thread handler))
        (parameterize ([current-directory main-dir])
          (apply compile-files/scv-cr (map path->string targets)))
        (define gc-string (open-output-string))
        (parameterize ([current-output-port gc-string])
          (continue buf initial-times
                    (cons (current-process-milliseconds)
                          (current-inexact-milliseconds))))
        (kill-thread thr)
        (enqueue! config-timing (list 'gc-stats #f (get-output-string gc-string)))
        (enqueue! (current-timing) (queue->list config-timing)))
      #:logger scv-cr-logger
      'info)

    ;; run
    (define resolved-main (make-resolved-module-path (string->path main)))
    (define iterations (config-ref config key:iterations))
    (for ([_ (in-range iterations)])
      (parameterize ([current-namespace (make-base-namespace)]
                     [current-output-port out-port])
        (dynamic-require resolved-main #f)))))

(define (parse-gtp-measure-data path benchmark timings)
  (define scv-cr-timings (hash-ref timings path))
  (with-input-from-file path
    (λ ()
      (void (read-line))
      (for/append ([ln (in-lines)]
                   #:when (non-empty-string? ln))
        (define ln-val (string->value ln))
        (define config (first ln-val))
        (for/list ([time (in-list (second ln-val))]
                   [scv-cr-time (in-list scv-cr-timings)])
          (define merged (merge-timings scv-cr-time))
          (define summarized (summarize-timings merged))
          (match-define
            (list expand-cpu-time expand-real-time expand-gc-time)
            (hash-ref summarized 'expand (λ () (list 0 0 0))))
          (match-define
            (list compile-cpu-time compile-real-time compile-gc-time)
            (hash-ref summarized 'compile))
          (match-define
            (list analyze-cpu-time analyze-real-time analyze-gc-time)
            (hash-ref summarized 'analyze))
          (hash 'benchmark benchmark
                'config config
                'cpu-time (time-string->cpu-time time)
                'real-time (time-string->real-time time)
                'gc-time (time-string->gc-time time)
                'expand-cpu-time expand-cpu-time
                'expand-real-time expand-real-time
                'expand-gc-time expand-gc-time
                'compile-cpu-time compile-cpu-time
                'compile-real-time compile-real-time
                'compile-gc-time compile-gc-time
                'analyze-cpu-time analyze-cpu-time
                'analyze-real-time analyze-real-time
                'analyze-gc-time analyze-gc-time
                'gc-stats (format "~s" (hash-ref summarized 'gc-stats))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions

(define ((merge-id x))
  (cond
    [(string? x) ""]
    [(list? x) '(0 0 0)]))

(define ((merge-op x) y)
  (cond
    [(string? x) (string-append x y)]
    [(list? x) (map + x y)]))

(define (merge-timings timings)
  (for/fold ([acc (hash)])
            ([timing (in-list timings)])
    (match-define (list key path this-times)
      (cond
        [(eq? (car timing) 'gc-stats) timing]
        [else
         (match-define (list key path cpu real gc) timing)
         (list key path (list cpu real gc))]))
    (hash-update acc
                 (list key path)
                 (merge-op this-times)
                 (merge-id this-times))))

(define (summarize-timings merged)
  (for/fold ([acc (hash)])
            ([(key+path timing) (in-hash merged)])
    (match-define (list key path) key+path)
    (hash-update acc
                 key
                 (merge-op timing)
                 (merge-id timing))))

(define (measure-data task timings)
  (define out-dir (task->directory task))
  (define outs
    (filter (λ (x) (path-has-extension? x #".out"))
            (directory-list out-dir #:build? out-dir)))
  (define outs* (sort outs path<?))
  (append-map (parse-gtp-measure-data _ _ timings)
              outs*
              (sort BENCHMARKS string<?)))

(define (measure-data->table data)
  (define e (first data))
  (define header (sort (hash-keys e) symbol<?))
  (cons header
        (for/list ([datum (in-list data)])
          (map (λ (k) (hash-ref datum k)) header))))

(define (read/ctc str ctc)
  (define v (with-input-from-string str read))
  (if (ctc v)
    v
    (raise-argument-error THIS-SYM (format "~a" (object-name ctc)) str)))

(define (hash->immutable-hash h)
  (for/hash (((k v) (in-hash h)))
    (values k v)))

(define rkt? (path-has-extension? _ #".rkt"))

(define (fake-prefixed? target)
  (string-prefix? (path->string (file-name-from-path target)) "fake-"))
