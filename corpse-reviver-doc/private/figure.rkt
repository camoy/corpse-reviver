#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide overhead-grid
         exact-grid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require gtp-plot/configuration-info
         gtp-plot/performance-info
         gtp-plot/plot
         gtp-plot/sample-info
         json
         racket/hash
         racket/path
         racket/list
         racket/match
         racket/set
         threading)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; const

(define DATA-FILENAME-RX #rx".*_(.*)_(.*)\\.json")

(*GRID-X* #f)
(*GRID-Y* #f)
(*OVERHEAD-PLOT-HEIGHT* 200)
(*OVERHEAD-PLOT-WIDTH* 400)
(*GRID-NUM-COLUMNS* 2)

;(define analysis (make-paths->hash "analysis" paths))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public

;;
;;
(define (overhead-grid paths)
  (define runtimes (make-paths->hash "runtime" paths))
  (define (overhead-or-samples pi)
    (if (sample-info? pi)
        (samples-plot pi)
        (overhead-plot pi)))
  (define pis
    (for/list ([(benchmark paths) (in-hash runtimes)])
      (define pi (runtime-paths->performance-info benchmark paths))
      (if (exhaustive? pi) pi (runtime-paths->sample-info pi paths))))
  (grid-plot overhead-or-samples pis))

;;
;;
(define (exact-grid paths)
  (define runtimes (make-paths->hash "runtime" paths))
  (define pis
    (for/list ([(benchmark paths) (in-hash runtimes)])
      (runtime-paths->performance-info benchmark paths)))
  (grid-plot exact-runtime-plot pis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private

(define (runtime-paths->sample-info pi paths)
  (make-sample-info pi (runtime-paths->samples paths)))

(define (runtime-paths->performance-info benchmark paths)
  (define h (runtime-paths->hash paths))
  (define units (string-length (first (hash-keys h))))
  (define baseline-runtimes (hash-ref h (make-string units #\0)))
  (define typed-runtimes (hash-ref h (make-string units #\1)))
  (define config-infos (hash->configuration-infos h))
  (make-performance-info
   (string->symbol benchmark)
   #:src "."
   #:num-units units
   #:num-configurations (hash-count h)
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

;; [Listof Path] → [Hash String [Listof Natural]]
;;
(define (runtime-paths->hash paths)
  (define run-hashes (append-map json->hashes paths))
  (hash-collapse run-hashes))

;;
;;
(define (hash-collapse run-hashes)
  (define hashes
    (for/list ([run-hash (in-list run-hashes)])
      (match-define
        (hash-table ['config config] ['real real] _ ...)
        run-hash)
      (hash config (list real))))
  (apply hash-union #:combine append hashes))

;;
;;
(define (hash->configuration-infos h)
  (for/list ([(config runtimes) (in-hash h)])
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
