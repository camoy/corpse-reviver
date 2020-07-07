#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide fig:overhead-grid
         fig:exact-grid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require gtp-plot/configuration-info
         gtp-plot/performance-info
         gtp-plot/plot
         gtp-plot/sample-info
         json
         pict-abbrevs
         racket/hash
         racket/path
         racket/list
         racket/runtime-path
         racket/match
         racket/set
         rebellion/collection/multidict
         rebellion/collection/entry
         threading)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; const

(define DATA-FILENAME-RX #rx".*_(.*)_(.*)\\.json")
#;(define COLOR-SCHEME (list #xfdb863 #xb2abd2))
(define COLOR-SCHEME (list #xa6dba0 #xc2a5cf))
(define DARK-COLOR-SCHEME (list #x008837 #x7b3294))

(*OVERHEAD-PLOT-HEIGHT* 250)
(*OVERHEAD-PLOT-WIDTH* 800)
(*OVERHEAD-SHOW-RATIO* #f)
(*OVERHEAD-SAMPLES* 100)
(*OVERHEAD-MAX* 10)
(*OVERHEAD-FONT-FACE* "CMU Concrete")
(*TITLE-FACE* "Linux Libertine")
(*POINT-SIZE* 6)
(*POINT-ALPHA* 0.9)
(*AUTO-POINT-ALPHA?* #f)
(*GRID-X* #f)
(*GRID-Y* #f)
(*GRID-X-SKIP* 50)
(*GRID-Y-SKIP* 20)
(*FONT-SIZE* 22)
(*GRID-NUM-COLUMNS* 2)
(*OVERHEAD-COLOR-LEGEND?* #f)
(*COLOR-LEGEND?* #f)

(define ((scheme-converter scheme) k)
  (define k* (modulo k (length scheme)))
  (hex-triplet->color% (list-ref scheme k*)))

(*BRUSH-COLOR-CONVERTER* (scheme-converter COLOR-SCHEME))
(*PEN-COLOR-CONVERTER* (scheme-converter DARK-COLOR-SCHEME))

(define-runtime-path BASELINE-PATH "../baseline")
(define BASELINE (directory-list BASELINE-PATH #:build? BASELINE-PATH))

(define-runtime-path OPT-PATH "../opt")
(define OPT (directory-list OPT-PATH #:build? OPT-PATH))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public

(define ((make-grid-figure pi-fun plot) . datasets)
  (define dataset-pis
    (for/list ([dataset (in-list datasets)])
      (define runtimes (make-paths->hash "runtime" dataset))
      (define benchmarks (sort (hash-keys runtimes) string<=?))
      (define pis
        (for/list ([benchmark (in-list benchmarks)])
          (define paths (hash-ref runtimes benchmark))
          (with-handlers
            ([exn:fail:benchmark?
              (λ _ (error 'overhead-grid "benchmark ~a failed" benchmark))])
            (pi-fun (runtime-paths->performance-info benchmark paths)
                    benchmark
                    paths))))
      pis))
  (define dataset-pis* (apply map list dataset-pis))
  (grid-plot plot dataset-pis*))

;;
;;
(define overhead-grid
  (make-grid-figure
   (λ (pi benchmark paths)
     (if (exhaustive? pi) pi (runtime-paths->sample-info pi paths)))
   overhead-plot))

(define (overhead-or-samples pis)
  (if (sample-info? (first pis))
      (samples-plot pis)
      (overhead-plot pis)))

;;
;;
(define exact-grid (make-grid-figure (λ (pi . _) pi) exact-runtime-plot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private

(struct exn:fail:benchmark exn:fail ())

(define (runtime-paths->sample-info pi paths)
  (make-sample-info pi (runtime-paths->samples paths)))

(define (runtime-paths->performance-info benchmark paths)
  (define h (runtime-paths->hash paths))
  (define units (string-length (set-first (multidict-unique-keys h))))
  (define baseline-runtimes
    (set-first (multidict-ref h (make-string units #\0))))
  (define typed-runtimes
    (set-first (multidict-ref h (make-string units #\1))))
  (define config-infos (hash->configuration-infos h))
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
;; TODO
(define (runtime-paths->samples paths)
  (define run-hashes (append-map json->hashes paths))
  (define -hashes
    (for/fold ([acc (hash)])
              ([run-hash (in-list run-hashes)])
      (define sample (hash-ref run-hash 'sample))
      (hash-update acc sample (λ~>> (cons run-hash)) null)))
  (define hashes (remove-baseline -hashes))
  (for/list ([(k v) (in-hash hashes)])
    (hash->configuration-infos (hash-collapse v))))

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

;; [Listof Path] → [Multidict String [Listof Natural]]
;;
(define (runtime-paths->hash paths)
  (define run-hashes (append-map json->hashes paths))
  (hash-collapse run-hashes))

;;
;;
(define (hash-collapse hashes)
  (define grouped-hashes (group-by (λ~> (hash-ref 'config-id)) hashes))
  (for/multidict ([run-hashes (in-list grouped-hashes)])
    (define config (hash-ref (first run-hashes) 'config))
    (define times
      (for/list ([h (in-list run-hashes)])
        (match-define (hash-table ['real real] ['error err] _ ...) h)
        (when err
          (raise (exn:fail:benchmark err (current-continuation-marks))))
        real))
    (entry config times)))

;;
;;
(define (hash->configuration-infos h)
  (for/list ([e (in-multidict-entries h)])
    (define-values (config runtimes)
      (values (entry-key e) (entry-value e)))
    (configuration-info config (count-hi-bits config) runtimes)))

;; Path → [Listof [Hash Any Any]]
;; Convert a JSON file to a list of hashes with keys based on the header.
(define (json->hashes path)
  (with-input-from-file path read-json))

;; String [Listof Path] → [Hash String [List Path]]
;; Given the kind of data we're interested, creates a hash mapping benchmark
;; names to the paths providing that kind of data.
(define (make-paths->hash kind paths)
  (define hashes (map (make-path->hash kind) paths))
  (apply hash-union #:combine set-union hashes))

;; Path → [Hash Symbol [Set Path]]
;; Returns a hash mapping benchmarks and
(define ((make-path->hash kind) path)
  (define benchmark+kind (path->benchmark path))
  (if (and benchmark+kind (equal? kind (second benchmark+kind)))
      (hash (first benchmark+kind) (list path))
      (hash)))

;; Path → [Or #f [List String String]]
;; Returns the benchmark name and what kind of data it is.
(define (path->benchmark path)
  (define matches
    (~>> path file-name-from-path path->string (regexp-match DATA-FILENAME-RX)))
  (and matches (cdr matches)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; figures

(define fig:overhead-grid
  (overhead-grid BASELINE))

(define fig:exact-grid
  (exact-grid BASELINE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk)

  (define eg-data
    (string->path "somewhere/2020-05-19T11:05:10_sieve_analysis.json"))
  (define eg-runtime-0
    (string->path "somewhere/2020-05-19T11:05:10_sieve_runtime.json"))
  (define eg-runtime-1
    (string->path "somewhere/2020-05-19T11:05:15_sieve_runtime.json"))
  (define eg-all (list eg-data eg-runtime-0 eg-runtime-1))

  (with-chk (['name "path->benchmark"])
    (chk
     (path->benchmark eg-data)
     '("sieve" "analysis")))

  (with-chk (['name "make-path->hash"])
    (chk
     ((make-path->hash "analysis") eg-data)
     (hash "sieve" (list eg-data))

     ((make-path->hash "runtime") eg-data)
     (hash)))

  (with-chk (['name "make-paths->hash"])
    (chk
     #:eq set=?
     (hash-ref (make-paths->hash "runtime" eg-all) "sieve")
     (list eg-runtime-0 eg-runtime-1)))
  )
