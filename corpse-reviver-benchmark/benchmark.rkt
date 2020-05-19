#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [benchmark/scv-cr (-> any)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in rackunit require/expose)
         csv-writing
         corpse-reviver
         corpse-reviver/private/compile
         corpse-reviver/private/logging
         data/queue
         gtp-measure/private/configure
         gtp-measure/private/parse
         gtp-measure/private/task
         gtp-util
         json
         make-log-interceptor
         mischief/for
         racket/runtime-path
         racket/cmdline
         racket/date
         racket/exn
         racket/hash
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         racket/syntax
         racket/system
         threading)

(require/expose gtp-measure/private/task
                (copy-configuration!
                 pre-typed-untyped-subtask-tu-dir
                 pre-typed-untyped-subtask-config-dir
                 pre-typed-untyped-subtask-in-file*
                 pre-typed-untyped-subtask-out-file*
                 gtp-measure-task-dir
                 format-target-tag
                 in-pre-subtasks
                 INPUT-EXTENSION
                 make-gtp-measure-subtask
                 write-lang!))

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

;; [Listof Symbol]
;; Columns that come first in the CSV output.
(define CSV-HEADER-PRIORITY '(benchmark config))

;; Path
;; Useful local paths.
(define-runtime-path BENCHMARK-DIR "benchmarks")
(define-runtime-path TYPED-RACKET-DIR "private/benchmark-typed-racket")

;; [Hash Path Complete-Path]
;; Proxy hash for patching Typed Racket. This is needed because running the
;; benchmarks require some changes to Typed Racket.
(define PROXY-HASH
  (make-proxy-hash
   #hash((typed-racket/base-env/prims-contract . "prims-contract.rkt")
         (typed-racket/base-env/prims . "prims.rkt"))
   TYPED-RACKET-DIR))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameters

(define current-iterations (make-parameter 10))
(define current-cutoff (make-parameter 10))
(define current-no-skip? (make-parameter #f))
(define current-num-samples (make-parameter 4))
(define current-sample-factor (make-parameter 10))
(define current-output-dir (make-parameter "."))

(define current-benchmark (make-parameter #f))
(define current-analyses (make-parameter #f))
(define current-runtimes (make-parameter #f))

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
   [("-i" "--iterations") iters
                          "Number of iterations"
                          (current-iterations (string->number iters))]

   [("-c" "--cutoff") cutoff
                      "Maximum number of components to measure exhaustively"
                      (current-cutoff (string->number cutoff))]

   [("-n" "--no-skip") "Don't skip analysis of modules prefixed with _"
                       (current-no-skip? #t)]

   [("-S" "--sample-factor") sample-factor
                             "Sample factor for calculating the sample size"
                             (current-sample-factor
                              (string->number sample-factor))]

   [("-R" "--num-samples") num-samples
                           "Number of samples"
                           (current-num-samples (string->number num-samples))]

   [("-o" "--output") output-dir
                      "Result output directory"
                      (current-output-dir output-dir)]
   #:args targets
   targets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; benchmark/scv-cr

;; Path-String ... → Any
;; Benchmarks the given GTP typed/untyped targets.
(define/contract (benchmark/scv-cr . targets)
  (->* () #:rest (listof valid-typed-untyped-target?) any)
  (define config (make-config))
  (for ([target (in-list targets)])
    (define task (init-task `((,target . ,kind:typed-untyped)) config))
    (define benchmark (last-dir target))
    (define analyses (make-queue))
    (define runtimes (make-queue))
    (init-untyped-typed-subtasks! task target)
    (output-specs!)
    (parameterize ([current-benchmark benchmark]
                   [current-analyses analyses]
                   [current-runtimes runtimes])
      (for* ([pre-subtask (in-list (in-pre-subtasks task))]
             [subtask (in-list (pre-subtask->subtask* pre-subtask config))])
        (subtask-run! subtask))
      (output-csv! benchmark 'analysis analyses)
      (output-csv! benchmark 'runtime runtimes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subtasks

;; Task Path-String → Any
;; Generates two new subtasks for the given task for a fully untyped and fully
;; typed configuration. This is baseline information needed for plotting that
;; may be otherwise unavailable (if sampling).
(define (init-untyped-typed-subtasks! task target)
  (define task-dir (gtp-measure-task-dir task))
  (define base-filename (build-path task-dir (format-target-tag target 0)))
  (define num-components (typed-untyped->num-components target))
  (define (in-file subtask char)
    (define filename
      (~> base-filename
          (path-add-extension (string-append "-" subtask))
          (path-add-extension INPUT-EXTENSION)))
    (with-output-to-file filename
      (λ () (displayln (make-string num-components char)))))
  (in-file "untyped" #\0)
  (in-file "typed" #\1))

;; Pre-Subtask Config → [Listof Subtask]
;; Generates subtasks from a pre-subtask. Taken directly out of the `gtp-measure`
;; package.
(define (pre-subtask->subtask* pst config)
  (define tu-dir (pre-typed-untyped-subtask-tu-dir pst))
  (define config-dir (pre-typed-untyped-subtask-config-dir pst))
  (define/for/lists (in-files out-files)
    ([in-file (in-list (pre-typed-untyped-subtask-in-file* pst))]
     [out-file (in-list (pre-typed-untyped-subtask-out-file* pst))]
     #:when (not (file-exists? out-file)))
    (values in-file out-file))
  (typed-untyped->subtask* tu-dir config-dir in-files out-files config))

;; Path Path [Listof Path] [Listof Path] Config → [Listof Subtask]
;; Given some task information, generates a list of runnable subtasks. This is a
;; modified version from `gtp-measure` that uses our custom runner thunk that
;; keeps track of analysis and runtime information independently of the out
;; files.
(define (typed-untyped->subtask* tu-dir config-dir ins outs config)
  (define entry
    (~> config
        (config-ref key:entry-point)
        (build-path config-dir _)
        path->string))
  (for/list ([in (in-list ins)]
             [out (in-list outs)])
    (define (thunk [out-port (current-output-port)])
      (write-lang! out-port "typed-untyped")
      (with-input-from-file in
        (λ ()
          (for ([config-id (in-lines)]
                [cfg-i (in-naturals 1)])
            (copy-configuration! config-id tu-dir config-dir)
            (define-values (config-in config-out) (make-pipe))
            (define-values (analysis-times base-running-times)
              (typed-untyped-run! entry config config-out config-id))
            (close-output-port config-out)
            (define raw-running-times (port->lines config-in))
            (writeln (list config-id raw-running-times) out-port)
            (define running-times
              (map (λ~> time-string->list times->hash) raw-running-times))
            (for ([time (in-list running-times)])
              (enqueue! (current-runtimes)
                        (hash-union time
                                    base-running-times
                                    #:combine (λ (x _) x))))
            (enqueue! (current-analyses) analysis-times)
            (when (empty? running-times)
              (enqueue! (current-runtimes) base-running-times))
            (close-input-port config-in)))))
    (make-gtp-measure-subtask out thunk config)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; typed-untyped-run!

;; Path Config Path Natural → [Hash Symbol Any] [Hash Symbol Any]
;; Given subtask information, populates hashs containing information from running
;; that subtask. This is for both analysis and run-time.
(define (typed-untyped-run! entry config config-out config-id)
  (define dir (path-only entry))
  (define targets (filter relevant-target? (directory-list dir #:build? dir)))
  (define base `((benchmark . ,(current-benchmark))
                 (config . ,config-id)
                 (error . #f)))
  (define analysis-times (make-hash base))
  (define running-times
    (make-hash (append base '((real . 0) (cpu . 0) (gc . 0)))))
  (parameterize ([current-output-port config-out]
                 [current-directory dir])
    (when (typed-untyped-compile! targets analysis-times)
      (typed-untyped-execute! entry config running-times)))
  (values analysis-times running-times))

;; [Listof Path] [Hash Symbol Any] → Boolean
;; Compiles the given targets and populates the analysis time hash. It returns
;; whether the compilation was successful.
(define (typed-untyped-compile! targets analysis-times)
  (define interceptor (make-log-interceptor scv-cr-logger))
  (define-values (_ logs)
    (interceptor
     (λ ()
       (with-handlers ([exn:fail? (make-error-handler analysis-times)])
         (apply compile-files/scv-cr (map path->string targets))))))
  (define times (map read-message (hash-ref logs 'info)))
  (hash-union! analysis-times
               (sum-times 'compile times)
               (sum-times 'expand times)
               (sum-times 'analyze times)
               (hash 'blame (and~>> times (findf (has-tag? 'blame)) cdr))
               #:combine (λ (_ x) x))
  (not (hash-ref analysis-times 'error)))

;; Path Config → Any
;; Runs the program and populates the run-time hash with failure information.
;; Notice that the run-time hash has no performance data yet. This happens in
;; `typed-untyped->subtask*` instead.
(define (typed-untyped-execute! entry config running-times)
  (define resolved-entry (make-resolved-module-path (string->path entry)))
  (define iterations (config-ref config key:iterations))
  (for ([_ (in-range iterations)])
    (with-patched-typed-racket
      (λ ()
        (with-handlers ([exn:fail? (make-error-handler running-times)])
          (dynamic-require resolved-entry #f)))
      PROXY-HASH)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; output

;; → Any
;; Outputs machine specification information (Unix only). Specifically data about
;; CPU and memory.
(define (output-specs!)
  (define filename (format "~a_specs.txt" (timestamp)))
  (with-output-to-file (build-path (current-output-dir) filename)
    (λ ()
      (system "cat /proc/cpuinfo /proc/meminfo"))))

;; String Symbol Queue → Any
;; Given a queue of data, output this to a timestamped CSV file.
(define (output-csv! benchmark key queue)
  (define filename (format "~a_~a_~a.csv" (timestamp) benchmark key))
  (with-output-to-file (build-path (current-output-dir) filename)
    (λ ()
      (display-table (list->table (queue->list queue))))))

;; [Listof Any] → Table
;; Given a list of values, convert this to a writable table.
(define (list->table xs)
  (define header (sort-header (hash-keys (first xs))))
  (cons header
        (for/list ([x (in-list xs)])
          (map (λ~>> (hash-ref x) ->js) header))))

;; Any → Any
;; Returns a representation suitable for output to CSV. If CSV can handle it
;; natively, don't do anything. Otherwise, serialize to JSON (usually for blame
;; results).
(define (->js x)
  (cond
    ;; simple
    [(or (string? x) (number? x) (boolean? x) (symbol? x)) x]
    ;; complex
    [else
     (jsexpr->string
      (let go ([x x])
        (cond
          [(symbol? x) (symbol->string x)]
          [(list? x) (map go x)]
          [else x])))]))

;; [Listof Symbol] → [Listof Symbol]
;; Sort the CSV header according to the priority constant.
(define (sort-header header)
  (define header* (remove* CSV-HEADER-PRIORITY header))
  (append CSV-HEADER-PRIORITY (sort header* symbol<?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; util

;; → String
;; Get the current time in ISO-8601 format.
(define (timestamp)
  (parameterize ([date-display-format 'iso-8601])
    (date->string (current-date) #t)))

;; → Config
;; Creates a configuration from the command line argument parameters.
(define (make-config)
  (init-config (hash key:iterations (current-iterations)
                     key:cutoff (current-cutoff)
                     key:sample-factor (current-sample-factor)
                     key:num-samples (current-num-samples))))

;; String → [List Natural Natural Natural]
;; Parse a `time` string into a list of naturals.
(define (time-string->list str)
  (call-with-values
   (λ () (time-string->values str))
   list))

;; Symbol [Listof [Cons Symbol Any]] → [Hash String Natural]
;; Reduces down time lists that are tagged with `tag` by adding them together.
;; Returns a hash containing the result.
(define (sum-times tag times)
  (define match? (has-tag? tag))
  (define reduced-times
    (for/fold ([acc (list 0 0 0)])
              ([time (in-list times)])
      (if (match? time)
          (map + acc (cdr time))
          acc)))
  (times->hash reduced-times tag))

;; [Listof [Cons Symbol Any]] {Symbol} → [Hash String Natural]
;; Given a `time` list, returns a hash with that data.
(define (times->hash times [key-prefix #f])
  (define (symbol->key sym)
    (if key-prefix (format-symbol "~a-~a" key-prefix sym) sym))
  (match-define (list real cpu gc) times)
  (hash (symbol->key 'real) real
        (symbol->key 'cpu) cpu
        (symbol->key 'gc) gc))
;; Symbol → ([Cons Symbol Any] → Boolean)
;; Returns if a cons cell has a certain tag.
(define ((has-tag? tag) datum)
  (eq? (car datum) tag))

;; String → Any
;; Returns the datum out of an SCV-CR logger message string.
(define (read-message str)
  (define log-rx #rx"^scv-cr: (.+)$")
  (~>> str
       (regexp-match log-rx)
       second
       open-input-string
       read))

;; Path → Symbol
;; Returns the last directory of a path as a symbol.
(define (last-dir path)
  (define-values (_ name __) (split-path path))
  (string->symbol (path->string name)))

;; [Hash Symbol Any] → (Exception → Any)
;; Make an exception handler that sets up an error entry in a hash.
(define ((make-error-handler hash) e)
  (hash-set! hash 'error (exn->string e)))

;; Path → Boolean
;; Returns if a target is relevant (i.e. is a Racket file and unless
;; `current-no-skip?` is `#t`, ignores _ prefixed files).
(define (relevant-target? target)
  (and (path-has-extension? target #".rkt")
       (or (current-no-skip?) (not (underscore-prefixed? target)))))

;; Path → Boolean
;; Returns if the filename at the given path has an underscore prefix.
(define (underscore-prefixed? target)
  (~> target file-name-from-path path->string (string-prefix? "_")))
