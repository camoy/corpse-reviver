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
         corpse-reviver
         corpse-reviver/private/logging
         data/queue
         gtp-measure/private/configure
         gtp-measure/private/parse
         gtp-measure/private/task
         gtp-util
         make-log-interceptor
         mischief/for
         racket/bool
         racket/cmdline
         racket/exn
         racket/hash
         racket/list
         racket/path
         racket/port
         racket/string
         threading)

(require/expose gtp-measure/private/task
                (copy-configuration!
                 count-configurations
                 pre-typed-untyped-subtask-tu-dir
                 pre-typed-untyped-subtask-config-dir
                 pre-typed-untyped-subtask-in-file*
                 pre-typed-untyped-subtask-out-file*
                 gtp-measure-subtask-out
                 gtp-measure-subtask-config
                 gtp-measure-task-dir
                 format-target-tag
                 in-pre-subtasks
                 INPUT-EXTENSION
                 make-gtp-measure-subtask
                 write-lang!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameters

(define current-iterations (make-parameter 10))
(define current-cutoff (make-parameter 10))
(define current-skip? (make-parameter #f))
(define current-num-samples (make-parameter 4))
(define current-sample-factor (make-parameter 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

(module+ main
  (define args (parse (current-command-line-arguments)))
  (apply benchmark/scv-cr args))

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

   [("-s" "--skip") "Skip analysis of modules prefixed with _"
                    (current-skip? #t)]

   [("-S" "--sample-factor") sample-factor
                             "Sample factor for calculating the sample size"
                             (current-sample-factor
                              (string->number sample-factor))]

   [("-R" "--num-samples") num-samples
                           "Number of samples"
                           (current-num-samples (string->number num-samples))]
   #:args targets
   targets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public

;; Path-String ... → Any
(define (benchmark/scv-cr . targets)
  (define config (make-config))
  (for ([-target (in-list targets)])
    (define target (path->string (normalize-path -target)))
    (define kind (and (valid-typed-untyped-target? target) kind:typed-untyped))
    (define target+kind (cons target kind))
    (define task (init-task (list target+kind) config))
    (init-untyped-typed-subtasks task target)
    (for* ([pre-subtask (in-list (in-pre-subtasks task))]
           [subtask (in-list (pre-subtask->subtask* pre-subtask config))])
      (subtask-run! subtask))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private

;; → Config
;; Creates a configuration from the command line argument parameters.
(define (make-config)
  (init-config (hash key:iterations (current-iterations)
                     key:cutoff (current-cutoff)
                     key:sample-factor (current-sample-factor)
                     key:num-samples (current-num-samples))))

;; Task Path-String → Any
;; TODO
(define (init-untyped-typed-subtasks task target)
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
;; TODO
(define (pre-subtask->subtask* pst config)
  (define tu-dir (pre-typed-untyped-subtask-tu-dir pst))
  (define config-dir (pre-typed-untyped-subtask-config-dir pst))
  (define/for/lists (in-file* out-file*)
    ([in-file (in-list (pre-typed-untyped-subtask-in-file* pst))]
     [out-file (in-list (pre-typed-untyped-subtask-out-file* pst))]
     #:when (not (file-exists? out-file)))
      (values in-file out-file))
  (typed-untyped->subtask* tu-dir config-dir in-file* out-file* config))

;;
;; TODO
(define (typed-untyped->subtask* tu-dir config-dir ins outs config)
  (define entry
    (path->string (build-path config-dir (config-ref config key:entry-point))))
  (for/list ([in (in-list ins)]
             [out (in-list outs)])
    (define (thunk [out-port (current-output-port)])
      (write-lang! out-port "typed-untyped")
      (define total-configs (count-configurations in))
      (with-input-from-file in
        (λ ()
          (for ([config-id (in-lines)]
                [cfg-i (in-naturals 1)])
            (copy-configuration! config-id tu-dir config-dir)
            (define-values (config-in config-out) (make-pipe))
            (define-values (analysis-times running-times)
              (typed-untyped-run! entry config config-out))
            (close-output-port config-out)
            (define config-running-times (port->lines config-in))
            (writeln (list config-id config-running-times) out-port)
            (hash-set! running-times 'times
                       (map time-string->list config-running-times))
            (writeln analysis-times)
            (writeln running-times)
            (close-input-port config-in)))))
    (make-gtp-measure-subtask out thunk config)))

;; → Hash Hash
;; TODO
(define (typed-untyped-run! entry config config-out)
  (define dir (path-only entry))
  (define targets (filter relevant-target? (directory-list dir #:build? dir)))
  (define analysis-times (make-hash))
  (define running-times (make-hash))
  (parameterize ([current-output-port config-out]
                 [current-directory dir])
    (when (typed-untyped-compile! targets analysis-times)
      (typed-untyped-execute! entry config running-times)))
  (values analysis-times running-times))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile

;; → Boolean
;; TODO
(define (typed-untyped-compile! targets analysis-times)
  (define interceptor (make-log-interceptor scv-cr-logger))
  (define-values (_ logs)
    (interceptor
     (λ ()
       (with-handlers ([exn:fail? (make-error-handler analysis-times)])
         (apply compile-files/scv-cr (map path->string targets))))))
  (define times (map read-message (hash-ref logs 'info)))
  (hash-union! analysis-times
               (hash 'compile (reduce-by 'compile times)
                     'expand (reduce-by 'expand times)
                     'analyze (reduce-by 'analyze times)
                     'blame (findf (has-tag? 'blame) times))))

;;
;; TODO
(define (typed-untyped-execute! entry config running-times)
  (define resolved-entry (make-resolved-module-path (string->path entry)))
  (define iterations (config-ref config key:iterations))
  (for ([_ (in-range iterations)])
    (parameterize ([current-namespace (make-base-namespace)])
      (with-handlers ([exn:fail? (make-error-handler running-times)])
        (dynamic-require resolved-entry #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; util

;;
;; TODO
(define (time-string->list str)
  (call-with-values
   (λ () (time-string->values str))
   list))

;;
;; TODO
(define (reduce-by tag times)
  (define match? (has-tag? tag))
  (for/fold ([acc (list 0 0 0)])
            ([time (in-list times)])
    (if (match? time)
        (map + acc (cdr time))
        acc)))

;;
;; TODO
(define ((has-tag? tag) datum)
  (eq? (car datum) tag))

;;
;; TODO
(define (read-message str)
  (define log-rx #rx"^scv-cr: (.+)$")
  (~>> str
       (regexp-match log-rx)
       second
       open-input-string
       read))

;;
;; TODO
(define ((make-error-handler hash) e)
  (hash-set! hash 'error (exn->string e)))

;;
;; TODO
(define (relevant-target? target)
  (and (path-has-extension? target #".rkt")
       (implies (current-skip?) (not (underscore-prefixed? target)))))

;;
;; TODO
(define (underscore-prefixed? target)
  (~> target file-name-from-path path->string (string-prefix? "_")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk
           rackunit)

  )
