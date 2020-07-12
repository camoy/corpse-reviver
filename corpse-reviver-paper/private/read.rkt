#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide dir->pi-hash
         hash->sorted-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require gtp-plot/sample-info
         gtp-plot/performance-info
         gtp-plot/configuration-info
         json
         threading
         racket/list
         racket/function
         racket/match
         racket/string
         racket/path
         racket/set
         racket/hash
         mischief/for)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; consts

;; Regexp
;; Regular expression for recognizing benchmark output JSON files.
(define DATA-FILENAME-RX #rx".*_(.*)_(.*)\\.json")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public

;; Path → [Hash String Performance-Info]
;; Given a directory path, returns a hash mapping benchmark names to performance
;; info structures.
(define (dir->pi-hash dir)
  (define benchmark-hash (dir->benchmark-hash dir "runtime"))
  (for/hash ([(benchmark path) (in-hash benchmark-hash)])
    (values benchmark
            (path->pi path benchmark))))

;; [Hash A Any] {(A → A)} → [Listof Any]
;; Given a hash, returns a list of values sorted by key.
(define (hash->sorted-list h [<: string<=?])
  (define ks (sort (hash-keys h) <:))
  (for/list ([k (in-list ks)])
    (hash-ref h k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private

;; Path String → Performance-Info
;; Given a path to a runtime JSON output and benchmark name, returns a
;; performance info structure for that benchmark.
(define (path->pi path benchmark)
  (define run-hashes (json-path->hashes path))
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
  (define exhaustive? (= (expt 2 units) num-configurations))
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
  (if exhaustive?
      pi
      (make-sample-info pi (run-hashes->sample-cis run-hashes*))))

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

;; Path String → [Hash String Path]
;; Given a directory and a kind of output file, returns a hash mapping
;; benchmark names to the output file matching the kind.
(define (dir->benchmark-hash dir kind)
  (define paths (directory-list dir #:build? dir))
  (for*/hash ([path (in-list paths)]
              [benchmark (in-value (path->benchmark path kind))]
              #:when benchmark)
    (values benchmark path)))

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
;; test

(module+ test
  (require chk
           racket/runtime-path)

  (define-runtime-path BASELINE-PATH "../baseline")
  (define-runtime-path OPT-PATH "../opt")
  (define eg-data
    (string->path "somewhere/2020-05-19T11:05:10_sieve_analysis.json"))

  (with-chk (['name "dir->benchmark-hash"])
    (chk
     #:eq set=?
     (hash-keys (dir->benchmark-hash BASELINE-PATH "analysis"))
     '("fsm" "gregor" "kcfa" "lnm" "morsecode" "sieve" "snake" "suffixtree"
             "synth" "tetris" "zombie" "zordoz")))

  (with-chk (['name "path->benchmark"])
    (chk
     (path->benchmark eg-data "analysis")
     "sieve"
     #:! #:t (path->benchmark eg-data "runtime")))
  )
