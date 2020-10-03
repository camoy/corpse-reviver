#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide BASELINE-PIS
         OPT-PIS
         ANALYSES
         exhaustive?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require gtp-plot/sample-info
         gtp-plot/performance-info
         gtp-plot/configuration-info
         json
         threading
         racket/runtime-path
         racket/list
         racket/function
         racket/match
         racket/string
         racket/format
         racket/path
         racket/set
         racket/hash
         mischief/for
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; consts

;; Regexp
;; Regular expression for recognizing benchmark output JSON files.
(define DATA-FILENAME-RX #rx".*_(.*)_(.*)\\.json")

;; Path
;; TODO
(define-runtime-path BASELINE-DIR "../data/baseline")

;; Path
;; TODO
(define-runtime-path OPT-DIR "../data/opt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public

;; Path → [Hash String Performance-Info]
;; Given a directory path, returns a hash mapping benchmark names to performance
;; info structures.
(define (dir->pi-hash dir)
  (define benchmark-hash (dir->benchmark-hashes dir "runtime"))
  (for/hash ([(benchmark run-hashes) (in-hash benchmark-hash)])
    (values benchmark (path->pi run-hashes benchmark))))

;; [Hash A Any] {(A → A)} → [Listof Any]
;; Given a hash, returns a list of values sorted by key.
(define (hash->sorted-list h [<: string<=?])
  (define ks (sort (hash-keys h) <:))
  (for/list ([k (in-list ks)])
    (hash-ref h k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private

;; Hash String → Performance-Info
;; Given a path to a runtime JSON output and benchmark name, returns a
;; performance info structure for that benchmark.
(define (path->pi run-hashes benchmark)
  (define-values (typed-run-hashes untyped-run-hashes run-hashes*)
    (unpack-run-hashes run-hashes))
  (define units (~> run-hashes* first (hash-ref 'config) string-length))
  (define num-configurations
    (set-count
     (for/set ([run-hash (in-list run-hashes*)])
       (hash-ref run-hash 'config))))
  (define typed-runtimes (run-hashes->runtimes typed-run-hashes))
  (define untyped-runtimes (run-hashes->runtimes untyped-run-hashes))
  (define cis (run-hashes->cis run-hashes*))
  (define pi
    (make-performance-info
     (string->symbol benchmark)
     #:src "."
     #:num-units units
     #:num-configurations num-configurations
     #:baseline-runtime* untyped-runtimes
     #:untyped-runtime* untyped-runtimes
     #:typed-runtime* typed-runtimes
     #:make-in-configurations (const cis)))
  (if (exhaustive? pi)
      pi
      (make-sample-info pi (run-hashes->sample-cis run-hashes*))))

;; Performance-Info → Boolean
;; Returns if the performance info is exhaustive.
(define (exhaustive? pi)
  (= (expt 2 (performance-info->num-units pi))
     (performance-info->num-configurations pi)))

;; [Listof Hash] → [Listof Hash] [Listof Hash] [Listof Hash]
;; Partitions out run hashes into the "guaranteed" configurations for the typed
;; and untyped variants, and then the rest of the runs.
(define (unpack-run-hashes run-hashes)
  (define/for/fold ([typed-run-hashes null]
                    [untyped-run-hashes null]
                    [run-hashes* null])
                   ([run-hash (in-list run-hashes)])
    (define k (hash-ref run-hash 'sample))
    (values (if (= k 0) (cons run-hash typed-run-hashes) typed-run-hashes)
            (if (= k 1) (cons run-hash untyped-run-hashes) untyped-run-hashes)
            (if (> k 1) (cons run-hash run-hashes*) run-hashes*)))
  (values typed-run-hashes untyped-run-hashes (reverse run-hashes*)))

;; [Listof Hash] → [Listof [Listof Configuration-Info]]
;; Partitions the run hashes based on the sample.
(define (run-hashes->sample-cis run-hashes)
  (map run-hashes->cis
       (group-by (λ~> (hash-ref 'sample)) run-hashes)))

;; [Listof Hash] → [Listof Configuration-Info]
;; Returns configuration info structures given run hashes.
(define (run-hashes->cis run-hashes)
  (define grouped-hashes (group-by (λ~> (hash-ref 'config-id)) run-hashes))
  (for/list ([run-hashes (in-list grouped-hashes)])
    (define config (hash-ref (first run-hashes) 'config))
    (configuration-info config
                        (count-one-bits config)
                        (run-hashes->runtimes run-hashes))))

;; [Listof Hash] → [Listof Real]
;; Given a list of run hashes for the same configuration, returns all the
;; runtimes performance numbers.
(define (run-hashes->runtimes run-hashes)
  (for/list ([run-hash (in-list run-hashes)])
    (match-define (hash-table ['real real] ['error err] _ ...) run-hash)
    (when err (error 'list->performance-hash "runtime error ~a" err))
    real))

;; String → Natural
;; Returns the number of "one" bits in a bitstring.
(define (count-one-bits str)
  (for/sum ([c (in-string str)]
            #:when (eq? c #\1))
    1))

;; Path → [Listof Hash]
;; Convert a JSON file to a list of hashes with keys based on the header.
(define (json-path->hashes path)
  (with-input-from-file path read-json))

;; Path String → [Hash String Hash]
;; Given a directory and a kind of output file, returns a hash mapping
;; benchmark names to the hashes for that benchmark.
(define (dir->benchmark-hashes dir kind)
  (define hashes
    (~>> (directory-list dir #:build? dir)
         (filter (curryr path->benchmark kind))
         (append-map json-path->hashes)
         (group-by (curryr hash-ref 'benchmark))))
  (for/hash ([hs (in-list hashes)])
    (values (hash-ref (first hs) 'benchmark) hs)))

;; Path → [Or #f String]
;; Returns the benchmark name if it matches the given kind.
(define (path->benchmark path kind)
  (define matches
    (~>> path
         file-name-from-path
         path->string
         (regexp-match DATA-FILENAME-RX)))
  (cond
    [matches
     (match-define (list _ benchmark kind*) matches)
     (and (equal? kind kind*) benchmark)]
    [else #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; performance infos

(define BASELINE-PIS (hash->sorted-list (dir->pi-hash BASELINE-DIR)))
(define OPT-PIS (hash->sorted-list (dir->pi-hash OPT-DIR)))
(define ANALYSES (hash->sorted-list (dir->benchmark-hashes OPT-DIR "analysis")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk
           racket/runtime-path)

  (define-runtime-path BASELINE-PATH "../baseline")
  (define-runtime-path OPT-PATH "../opt")
  (define eg-data
    (string->path "somewhere/2020-05-19T11:05:10_sieve_analysis.json"))

  (with-chk (['name "dir->benchmark-hashes"])
    (chk
     #:eq set=?
     (hash-keys (dir->benchmark-hashes BASELINE-PATH "analysis"))
     '("fsm" "gregor" "kcfa" "lnm" "morsecode" "sieve" "snake" "suffixtree"
             "synth" "tetris" "zombie" "zordoz")))

  (with-chk (['name "path->benchmark"])
    (chk
     (path->benchmark eg-data "analysis")
     "sieve"
     #:! #:t (path->benchmark eg-data "runtime")))
  )
