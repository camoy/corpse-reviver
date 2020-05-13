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
         racket/date
         racket/cmdline
         racket/exn
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
    ("fsm" . ("benchmark-util.rkt"))
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
(define THIS-SYM 'scv-cr-benchmark)
(define THIS-STR "scv-cr-benchmark")
(define-runtime-path CWD ".")
(define BENCHMARK-DIR (build-path CWD "benchmarks"))
(define current-target (make-parameter #f))
(define current-timing (make-parameter #f))
(define current-config-timing (make-parameter #f))
(define current-ignore (make-parameter '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(module+ main
  (define config (parse (current-command-line-arguments)))
  (for ([b (in-list BENCHMARKS)])
    (define-values (benchmark ignore)
      (if (cons? b)
          (values (car b) (cdr b))
          (values b null)))
    (define target (build-path BENCHMARK-DIR benchmark))
    (measure-task benchmark ignore target config)))

;; [Vector String] → List
;; Converts command line arguments into arguments suitable for a call to
;; compile-files/scv-cr.
(define (parse argv)
  (define config
    (make-hash (list (cons key:bin (path->string (build-path CWD "bin"))))))
  (command-line
   #:program "scv-cr-benchmark"
   #:argv argv
   #:once-each
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

;; Path-String Config → Task [Hash Path-String Timing]
(define (measure-task benchmark ignore tgt config)
  (define target*
    (cons (path->string (normalize-path tgt))
          (valid-target? tgt)))
  (define config* (init-config (hash->immutable-hash config)))
  (define task (init-task (list target*) config*))
  (for ([cfg (in-list (task->config* task))])
    (check-pkg-deps (config-ref cfg key:bin) #:auto? #true))
  (for ([st (in-list (in-subtasks task))])
    (define timing (make-queue))
    (parameterize ([current-file-runner file-runner]
                   [current-target tgt]
                   [current-timing timing]
                   [current-ignore ignore])
      (subtask-run! st))
    (define timestamp
      (parameterize ([date-display-format 'iso-8601])
        (date->string (current-date) #t)))
    (with-output-to-file (format "~a_~a.csv" timestamp benchmark)
      (λ ()
        (display-table
         (measure-data->table
          (measure-data (gtp-measure-subtask-out st)
                        benchmark
                        (queue->list timing)))))
      #:exists 'replace)))

;; Path-String Config → Any
(define (file-runner main config)
  (define main-dir (path-only (path->complete-path (string->path main))))
  (define targets
    (filter (λ (x) (and (rkt? x)
                        (not (fake-prefixed? x))
                        (not (member (path->string (file-name-from-path x))
                                     (current-ignore)))))
            (directory-list main-dir #:build? main-dir)))
  (define should-try? #t)
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
          (with-handlers ([exn:fail? (λ (e)
                                       (set! should-try? #f)
                                       (enqueue! config-timing
                                                 (list 'error-scv-cr #f (exn->string e))))])
            (apply compile-files/scv-cr (map path->string targets))))
        (define gc-string (open-output-string))
        (parameterize ([current-output-port gc-string])
          (continue buf initial-times
                    (cons (current-process-milliseconds)
                          (current-inexact-milliseconds))))
        (kill-thread thr)
        (enqueue! config-timing (list 'gc-stats #f (get-output-string gc-string))))
      #:logger scv-cr-logger
      'info)

    ;; run
    (define resolved-main (make-resolved-module-path (string->path main)))
    (define iterations (config-ref config key:iterations))
    (when should-try?
      (for ([_ (in-range iterations)])
        (parameterize ([current-namespace (make-base-namespace)]
                       [current-directory (path-only (string->path main))]
                       [current-output-port out-port])
          (with-handlers ([exn:fail? (λ (e)
                                       (enqueue! config-timing
                                                 (list 'error-run #f (exn->string e))))])
            (dynamic-require resolved-main #f)))))
    (enqueue! (current-timing) (queue->list config-timing))))

(define (parse-gtp-measure-data path benchmark scv-cr-timings)
  #;(define scv-cr-timings (hash-ref timings path))
  (with-input-from-file path
    (λ ()
      (void (read-line))
      (for/append ([ln (in-lines)]
                   [scv-cr-time (in-list scv-cr-timings)]
                   #:when (non-empty-string? ln))
        (define ln-val (string->value ln))
        (define config (first ln-val))
        (define times (if (empty? (second ln-val)) '(#f) (second ln-val)))
        (define merged (merge-timings scv-cr-time))
        (define summarized (summarize-timings merged))
        (match-define
          (list expand-cpu-time expand-real-time expand-gc-time)
          (hash-ref summarized 'expand (λ () (list 0 0 0))))
        (match-define
          (list compile-cpu-time compile-real-time compile-gc-time)
          (hash-ref summarized 'compile (λ () (list 0 0 0))))
        (match-define
          (list analyze-cpu-time analyze-real-time analyze-gc-time)
          (hash-ref summarized 'analyze (λ () (list 0 0 0))))
        (match-define
          (list total-cpu-time total-real-time total-gc-time)
          (hash-ref summarized 'total (λ () (list 0 0 0))))
        (for/list ([time (in-list times)])
          (hash 'benchmark benchmark
                'config config
                'error-scv-cr (escape-newline (hash-ref summarized 'error-scv-cr (λ () "")))
                'error-run (escape-newline (hash-ref summarized 'error-run (λ () "")))
                'cpu-time (and time (time-string->cpu-time time))
                'real-time (and time (time-string->real-time time))
                'gc-time (and time (time-string->gc-time time))
                'expand-cpu-time expand-cpu-time
                'expand-real-time expand-real-time
                'expand-gc-time expand-gc-time
                'compile-cpu-time compile-cpu-time
                'compile-real-time compile-real-time
                'compile-gc-time compile-gc-time
                'analyze-cpu-time analyze-cpu-time
                'analyze-real-time analyze-real-time
                'analyze-gc-time analyze-gc-time
                'total-cpu-time total-cpu-time
                'total-real-time total-real-time
                'total-gc-time total-gc-time
                'warning (escape-newline (hash-ref summarized 'warning (λ () "")))
                'blame (format "~s" (hash-ref summarized 'blame (λ () null)))
                'gc-stats (escape-newline (hash-ref summarized 'gc-stats))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions

(define ((merge-id x))
  (cond
    [(string? x) ""]
    [(timing-list? x) '(0 0 0)]
    [else null]))

(define ((merge-op x) y)
  (cond
    [(string? x) (string-append x "\n" y)]
    [(timing-list? x) (map + x y)]
    [else (cons x y)]))

(define (merge-timings timings)
  (for/fold ([acc (hash)])
            ([timing (in-list timings)])
    (match-define (list key path this) timing)
    (hash-update acc
                 (list key path)
                 (merge-op this)
                 (merge-id this))))

(define (summarize-timings merged)
  (for/fold ([acc (hash)])
            ([(key+path timing) (in-hash merged)])
    (match-define (list key path) key+path)
    (hash-update acc
                 key
                 (merge-op timing)
                 (merge-id timing))))

(define (measure-data out benchmark timings)
  (parse-gtp-measure-data out benchmark timings)
  #|
  (define out-dir (task->directory task))
  (define outs
    (filter (λ (x) (path-has-extension? x #".out"))
            (directory-list out-dir #:build? out-dir)))
  (define outs* (sort outs path<?))
  (append-map (parse-gtp-measure-data _ _ timings)
              outs*
              (sort BENCHMARKS string<?))
  |#)

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

(define (escape-newline str)
  (string-replace str "\n" "\\n"))

(define (timing-list? x)
  (and (list? x)
       (andmap number? x)
       (= (length x) 3)))
