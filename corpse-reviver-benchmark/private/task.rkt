#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide place-benchmark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in rackunit require/expose)
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
         racket/place
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
         threading
         "config.rkt"
         "gc.rkt")

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

(require/expose gtp-measure/private/configure (gtp-measure-data-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals

;; Path
;; Useful local paths.
(define-runtime-path TYPED-RACKET-DIR "benchmark-typed-racket")

;; [Hash Path Complete-Path]
;; Proxy hash for patching Typed Racket. This is needed because running the
;; benchmarks require some changes to Typed Racket.
(define PROXY-HASH
  (make-proxy-hash
   #hash((typed-racket/base-env/prims-contract . "prims-contract.rkt"))
   TYPED-RACKET-DIR))

;; [Or #f Config]
;; Global configuration for the current task.
(define cfg #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; place-benchmark

;; Place-Channel → Any
;; Benchmarks the given GTP typed/untyped target. The place channel must be sent
;; the target and then the config struct.
(define (place-benchmark pch)
  (define target (place-channel-get pch))
  (set! cfg (~>> pch
                 place-channel-get
                 vector->list
                 cdr ; drop prefab struct key
                 (apply config)))
  (define-values (gtp-config task benchmark)
    (if (config-resume? cfg)
        (target->params/resume target)
        (target->params/init target)))
  (define-values (analyses runtimes)
    (values (make-queue) (make-queue)))
  (set-config-cfg-id! cfg 1)
  (for*/fold
      ([n 1])
      ([pre-subtask (in-list (in-pre-subtasks task))]
       #:when #t
       [subtask (in-list (pre-subtask->subtask* pre-subtask gtp-config))])
    (set-config-sample! cfg n)
    (set-config-benchmark! cfg benchmark)
    (set-config-analyses! cfg analyses)
    (set-config-runtimes! cfg runtimes)
    (subtask-run! subtask)
    (add1 n))
  (output-json! benchmark 'analysis analyses)
  (output-json! benchmark 'runtime runtimes))

;; String → Config Task String
;; Returns the parameters needed to run a target for a newly initialized
;; benchmark.
(define (target->params/init target)
  (define gtp-config (make-config))
  (define task (init-task `((,target . ,kind:typed-untyped)) gtp-config))
  (init-untyped-typed-subtasks! task target)
  (values gtp-config task (last-dir target)))

;; Natural → Config Task String
;; Returns the parameters needed to run a target for an already existing
;; benchmark for resuming.
(define (target->params/resume target)
  (define resume-dir
    (build-path (gtp-measure-data-dir) (format "~a/" target)))
  (values (directory->config resume-dir)
          (resume-task resume-dir)
          (~> resume-dir
              (build-path "manifest.rkt")
              manifest->targets
              first
              car
              last-dir)))

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
     [out-file (in-list (pre-typed-untyped-subtask-out-file* pst))])
    ;; Get rid of results (otherwise the task won't run)
    (when (file-exists? out-file)
      (delete-file out-file))
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
          (for ([config-id (in-lines)])
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
              (enqueue! (config-runtimes cfg)
                        (hash-union time
                                    base-running-times
                                    #:combine (λ (x _) x))))
            (enqueue! (config-analyses cfg) analysis-times)
            (when (empty? running-times)
              (enqueue! (config-runtimes cfg) base-running-times))
            (close-input-port config-in)
            (set-config-cfg-id! cfg (add1 (config-cfg-id cfg)))))))
    (make-gtp-measure-subtask out thunk config)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; typed-untyped-run!

;; Path Config Path Natural → [Hash Symbol Any] [Hash Symbol Any]
;; Given subtask information, populates hashs containing information from running
;; that subtask. This is for both analysis and run-time.
(define (typed-untyped-run! entry config config-out config-str)
  (define dir (path-only entry))
  (define targets (filter relevant-target? (directory-list dir #:build? dir)))
  (define base `((benchmark . ,(config-benchmark cfg))
                 (config . ,config-str)
                 (config-id . ,(config-cfg-id cfg))
                 (sample . ,(config-sample cfg))
                 (error . #f)))
  (define analysis-times (make-hash base))
  (define running-times
    (make-hash (append base '((real . 0) (cpu . 0) (gc . 0)))))
  (parameterize ([current-output-port config-out]
                 [current-directory dir])
    (cond
      [(typed-untyped-compile! targets analysis-times)
       (typed-untyped-execute! entry config running-times)]
      [else (hash-set! running-times 'error "compile-time error")]))
  (values analysis-times running-times))

;; [Listof Path] [Hash Symbol Any] → Boolean
;; Compiles the given targets and populates the analysis time hash. It returns
;; whether the compilation was successful.
(define (typed-untyped-compile! -targets analysis-times)
  (define targets (map path->string -targets))
  (for-each delete-bytecode targets)
  (define (execute-thunk)
    (with-handlers ([exn:fail? (make-error-handler analysis-times)])
      (if (config-baseline? cfg)
          ;; HACK: Remove this (and all of private/benchmark-typed-racket) once TR
          ;; #837 is resolved.
          (with-patched-typed-racket
            (λ () (compile-files targets))
            PROXY-HASH)
          (compile-files/scv-cr targets))))
  (define interceptor (make-log-interceptor scv-cr-logger))
  (define (run)
    (define-values (_ logs)
      (interceptor execute-thunk))
    logs)
  (define-values (logs gc-hash)
    (if (config-gc-log? cfg)
        (measure-gc run)
        (values (run) (hash))))
  (define times (map read-message (hash-ref logs 'info)))
  (hash-union! analysis-times
               (sum-times 'compile times)
               (sum-times 'expand times)
               (sum-times 'analyze times)
               (sum-times 'total times)
               (hash 'blame (and~>> times (findf (has-tag? 'blame)) cdr))
               gc-hash
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
    (parameterize ([current-namespace (make-base-namespace)])
      (with-handlers ([exn:fail? (make-error-handler running-times)])
        ;; Collect garbage before running to have "clean" GC state
        (collect-garbage 'major)
        (dynamic-require resolved-entry #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; output

;; String Symbol Queue → Any
;; Given a queue of data, output this to a timestamped CSV file.
(define (output-json! benchmark key queue)
  (define filename (format "~a_~a_~a.json" (iso-timestamp) benchmark key))
  (with-output-to-file (build-path (config-output-dir cfg) filename)
    (λ ()
      (~> queue queue->list list->jsexpr write-json))))

;; [Listof Any] → JSExpr
;; Given a list of values, convert this to a JSON convertible value.
(define (list->jsexpr xs)
  (define keys (hash-keys (first xs)))
  (for/list ([x (in-list xs)])
    (for/hash ([(k v) (in-hash x)])
      (values k (->js v)))))

;; Any → Any
;; Returns a representation suitable for output to JSON. This requires mapping
;; some symbols to strings.
(define (->js x)
  (cond
    [(symbol? x) (symbol->string x)]
    [(list? x) (map ->js x)]
    [else x]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; util

;; → Config
;; Creates a configuration from the command line argument parameters.
(define (make-config)
  (init-config (hash key:iterations (config-iterations cfg)
                     key:cutoff (config-cutoff cfg)
                     key:sample-factor (config-sample-factor cfg)
                     key:num-samples (config-num-samples cfg))))

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
       (or (config-no-skip? cfg) (not (underscore-prefixed? target)))))

;; Path → Boolean
;; Returns if the filename at the given path has an underscore prefix.
(define (underscore-prefixed? target)
  (~> target file-name-from-path path->string (string-prefix? "_")))
