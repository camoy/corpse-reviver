#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require csv-reading
         gtp-plot/configuration-info
         gtp-plot/performance-info
         gtp-plot/plot
         racket/hash
         racket/path
         racket/list
         racket/match
         racket/set
         threading)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; const

(define DATA-FILENAME-RX #rx".*_(.*)_(.*)\\.csv")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public

(define (main paths)
  (define analysis (make-paths->hash "analysis" paths))
  (define runtimes (make-paths->hash "runtime" paths))
  (for/list ([(benchmark paths) (in-hash runtimes)])
    (displayln benchmark)
    (overhead-plot (runtime-paths->performance-info benchmark paths))))

(define (runtime-paths->performance-info benchmark paths)
  (define h (runtime-paths->hash paths))
  (define units (string-length (first (hash-keys h))))
  (define baseline-runtimes (hash-ref h (make-string units #\0)))
  (define typed-runtimes (hash-ref h (make-string units #\1)))
  (define config-infos
    (for/list ([(config runtimes) (in-hash h)])
      ;; TODO: types
      (configuration-info config 0 runtimes)))
  (make-performance-info
   (string->symbol benchmark)
   #:src "."
   #:num-units units
   #:num-configurations (hash-count h)
   #:baseline-runtime* baseline-runtimes
   #:untyped-runtime* baseline-runtimes
   #:typed-runtime* typed-runtimes
   #:make-in-configurations (λ _ config-infos)))

;; [Listof Path] → [Hash String [Listof Natural]]
;;
(define (runtime-paths->hash paths)
  (define run-hashes (append-map csv->hashes paths))
  (define hashes
    (for/list ([run-hash (in-list run-hashes)])
      (match-define
        (hash-table ["config" config] ["real" real] _ ...)
        run-hash)
      (hash config (list (string->number real)))))
  (apply hash-union #:combine append hashes))

;; Path → [Listof [Hash Any Any]]
;; Convert a CSV file to a list of hashes with keys based on the header.
(define (csv->hashes path)
  (define in (open-input-file path))
  (match-define `(*TOP* (row ,header ...) ,rows ...) (csv->sxml in))
  (close-input-port in)
  (for/list ([row (in-list (cdr rows))])
    (for/hash ([k+v (in-list (cdr row))])
      (match-define (list k v) k+v)
      (values (second (assoc k header)) v))))

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

(main '("/home/camoy/wrk/corpse-reviver/corpse-reviver-doc/data/2020-05-19T11:05:10_sieve_runtime.csv"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk)

  (define eg-data
    (string->path "somewhere/2020-05-19T11:05:10_sieve_analysis.csv"))
  (define eg-runtime-0
    (string->path "somewhere/2020-05-19T11:05:10_sieve_runtime.csv"))
  (define eg-runtime-1
    (string->path "somewhere/2020-05-19T11:05:15_sieve_runtime.csv"))
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
