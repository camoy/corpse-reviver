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
         rebellion/collection/entry
         rebellion/collection/multidict)

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

;; [Hash A Any] (A → A) → [Listof Any]
;; Given a hash, returns a list of values sorted by key.
(define (hash->sorted-list h <:)
  (define ks (sort (hash-keys h) <:))
  (for/list ([k (in-list ks)])
    (hash-ref h k)))

(require racket/pretty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private

;; Path String → Performance-Info
;; Given a path to a runtime JSON output and benchmark name, returns a
;; performance info structure for that benchmark.
(define (path->pi path benchmark)
  (define run-hashes (json-path->hashes path))
  (define runtime-dict (hashes->runtime-dict run-hashes))
  (define units
    (~> runtime-dict multidict-unique-keys set-first string-length))
  (define num-configurations
    (set-count (multidict-unique-keys runtime-dict)))
  (define baseline-runtimes
    (set-first (multidict-ref runtime-dict (make-string units #\0))))
  (define typed-runtimes
    (set-first (multidict-ref runtime-dict (make-string units #\1))))
  (define cis (runtime-dict->cis runtime-dict))
  (define exhaustive? (= (expt 2 units) num-configurations))
  (define pi
    (make-performance-info
     (string->symbol benchmark)
     #:src "."
     #:num-units units
     #:num-configurations num-configurations
     #:baseline-runtime* baseline-runtimes
     #:untyped-runtime* baseline-runtimes
     #:typed-runtime* typed-runtimes
     #:make-in-configurations (const cis)))
  (if exhaustive?
      pi
      (make-sample-info pi (run-hashes->sample-cis run-hashes))))

;; [Listof Hash] → [Listof [Listof Configuration-Info]]
;; Given a list of run hashes, returns a partition of configuration infos. We
;; filter out the baseline samples. Those should always be sample 0 and 1.
(define (run-hashes->sample-cis run-hashes)
  (define sample (λ~> (hash-ref 'sample)))
  (define run-hashes*
    (filter (λ (x) (>= (sample x) 2)) run-hashes))
  (map (λ~> hashes->runtime-dict runtime-dict->cis)
       (group-by sample run-hashes*)))

;; String → Natural
;; Returns the number of "one" bits in a bitstring.
(define (count-one-bits str)
  (for/sum ([c (in-string str)]
            #:when (eq? c #\1))
    1))

;; [Multidict String [Listof Real]] → [Listof Configuration-Info]
;; Given a runtime multidict, returns a list of configuration infos .
(define (runtime-dict->cis runtime-dict)
  (for/list ([e (in-multidict-entries runtime-dict)])
    (define-values (config runtimes)
      (values (entry-key e) (entry-value e)))
    (configuration-info config (count-one-bits config) runtimes)))

;; [Listof Hash] → [Multidict String [Listof Real]]
;; Given a list of runtime hashes, returns that information as a runtime
;; multidict (since in sampling we can repeat the same configuration).
(define (hashes->runtime-dict runtimes)
  (define grouped-hashes (group-by (λ~> (hash-ref 'config-id)) runtimes))
  (for/multidict ([run-hashes (in-list grouped-hashes)])
    (define config (hash-ref (first run-hashes) 'config))
    (define times
      (for/list ([h (in-list run-hashes)])
        (match-define (hash-table ['real real] ['error err] _ ...) h)
        (when err
          (error 'list->performance-hash "runtime error ~a" err))
        real))
    (entry config times)))

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


#|
(define (filter-benchmark paths benchmarks)
  (define (only-benchmark? path)
    (for/or ([benchmark (in-list benchmarks)])
      (string-contains? (path->string path) benchmark)))
  (filter only-benchmark? paths))

;; TODO: memoize this calculation

(define (runtime-paths->sample-info pi paths)
  (make-sample-info pi (runtime-paths->samples paths)))

(define (runtime-paths->performance-info benchmark paths)
  (define h (runtime-paths->hash paths))
  (define units (string-length (set-first (multidict-unique-keys h))))
  (define baseline-runtimes
    (set-first (multidict-ref h (make-string units #\0))))
  (define typed-runtimes
    (set-first (multidict-ref h (make-string units #\1))))
  (define config-infos (exhaustive-configuration-infos h))
  (make-performance-info
   (string->symbol benchmark)
   #:src "."
   #:num-units units
   #:num-configurations (set-count (multidict-unique-keys h))
   #:baseline-runtime* baseline-runtimes
   #:untyped-runtime* baseline-runtimes
   #:typed-runtime* typed-runtimes
   #:make-in-configurations (λ _ config-infos)))

;;
;;
(define (exhaustive? pi)
  (define num-units (performance-info->num-units pi))
  (define num-configs (performance-info->num-configurations pi))
  (= num-configs (expt 2 (performance-info->num-units pi))))

;; String → Natural
;;
(define (count-hi-bits cfg)
  (for/sum ([c (in-string cfg)]
            #:when (eq? c #\1))
    1))

;; [Listof Path] → [Listof [Listof Configuration-Info]]
;; TODO: Shouldn't remove baseline since may actually be in sample
;; only remove the first ones
(define (runtime-paths->samples paths)
  (define run-hashes (append-map json->hashes paths))
  (define -hashes
    (for/fold ([acc (hash)])
              ([run-hash (in-list run-hashes)])
      (define sample (hash-ref run-hash 'sample))
      (hash-update acc sample (λ~>> (cons run-hash)) null)))
  (define hashes (remove-baseline -hashes))
  (for/list ([(k v) (in-hash hashes)])
    (sample-configuration-infos (hash-collapse v))))

;;
;;
(define (remove-baseline hashes)
  (for/hash ([(k v) (in-hash hashes)]
             #:when (not (andmap baseline? v)))
    (values k v)))

;;
;;
(define (baseline? h)
  (define str (hash-ref h 'config))
  (define n (string-length str))
  (define hi (count-hi-bits str))
  (or (= hi 0) (= hi n)))

;;
;;
(define (sample-configuration-infos h)
  (for/list ([e (in-multidict-entries h)])
    (define-values (config runtimes)
      (values (entry-key e) (entry-value e)))
    (configuration-info config (count-hi-bits config) runtimes)))
|#
