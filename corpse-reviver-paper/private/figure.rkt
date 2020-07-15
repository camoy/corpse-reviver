#lang at-exp racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide overhead-summary
         lattices
         overhead-grid
         exact-grid
         table-summary)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require gtp-plot/configuration-info
         gtp-plot/performance-info
         gtp-plot/plot
         gtp-plot/sample-info
         (only-in gtp-util natural->bitstring)
         json
         math/statistics
         pict
         pict-abbrevs
         racket/stream
         racket/string
         racket/hash
         racket/function
         racket/sequence
         racket/format
         racket/path
         racket/list
         racket/runtime-path
         racket/match
         racket/set
         scribble/base
         threading
         "lattice.rkt"
         "read.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; consts

(define COLOR-SCHEME (list #xfdb863 #xb2abd2))
(define DARK-COLOR-SCHEME (list #xe66101 #x5e3c99))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameters

(*SAMPLE-INTERVAL-ALPHA* 0.6)
(*OVERHEAD-PLOT-HEIGHT* 275)
(*OVERHEAD-PLOT-WIDTH* 800)
(*OVERHEAD-SHOW-RATIO* #f)
(*OVERHEAD-SAMPLES* 100)
(*OVERHEAD-MAX* 10)
(*OVERHEAD-FONT-FACE* "CMU Concrete")
(*TITLE-FACE* "Linux Libertine")
(*POINT-COLOR* 1)
(*POINT-SIZE* 6)
(*POINT-ALPHA* 1)
(*AUTO-POINT-ALPHA?* #f)
(*GRID-X* #f)
(*GRID-Y* #f)
(*GRID-X-SKIP* 75)
(*GRID-Y-SKIP* 50)
(*FONT-SIZE* 32)
(*GRID-NUM-COLUMNS* 2)
(*OVERHEAD-COLOR-LEGEND?* #f)
(*COLOR-LEGEND?* #f)

(*LATTICE-BOX-HEIGHT* 26)
(*LATTICE-LEVEL-MARGIN* 14)
(*LATTICE-BOX-WIDTH* 14)
(*LATTICE-FONT-SIZE* 14)

;; [Listof Number] → (Natural → Color%)
;; Constructs a cyclical color converter.
(define ((make-color-converter scheme) k)
  (define k* (modulo k (length scheme)))
  (hex-triplet->color% (list-ref scheme k*)))

(*BRUSH-COLOR-CONVERTER* (make-color-converter COLOR-SCHEME))
(*PEN-COLOR-CONVERTER* (make-color-converter DARK-COLOR-SCHEME))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; summary figure

;; [Parameterof Real]
;;
(define *SUMMARY-GRANULARITY* (make-parameter 1/100))

;;
;; TODO
(define (make-overhead-summary . pis*)
  (define cdf-pis
    (for/list ([pis (in-list pis*)])
      (define cis (pis->cdf-cis pis))
      (make-performance-info
       '||
       #:src "."
       #:num-units 0
       #:num-configurations (length cis)
       #:baseline-runtime* '(1)
       #:untyped-runtime* '(1)
       #:typed-runtime* '(1)
       #:make-in-configurations (const cis))))
  (parameterize ([*OVERHEAD-SHOW-CONFIGURATIONS* #f])
    (overhead-plot cdf-pis)))

;;
;; TODO
(define (pis->cdf-cis pis)
  (define interval (in-range 1 (*OVERHEAD-MAX*) (*SUMMARY-GRANULARITY*)))
  (define size (/ (sub1 (*OVERHEAD-MAX*)) (*SUMMARY-GRANULARITY*)))
  (define inv (norm-inverse-cdf pis interval))
  (for/list ([ind (in-naturals 1)]
             [k interval])
    (configuration-info "" 0 (list (inv (/ ind size))))))

;;
;; TODO
(define (norm-inverse-cdf pis Ds)
  (define graph*
    (for/list ([(D %) (in-hash (norm-cdf Ds pis))])
      (cons % D)))
  (define graph
    (sort graph*
          (λ (x y)
            (define-values (x* y*) (values (car x) (car y)))
            (if (= x* y*) (< (cdr x) (cdr y)) (< x* y*)))))
  (λ (x)
    (define p (assf (λ (v) (>= v x)) graph))
    (if p (cdr p) 100)))

;;
;; TODO
(define (norm-cdf Ds pis)
  (define N (length pis))
  (define cdfs (map (norm-cdf* Ds) pis))
  (define non-normal-cdfs (apply hash-union #:combine + cdfs))
  (for/hash ([(D %) (in-hash non-normal-cdfs)])
    (values D (/ % N))))

;;
;; TODO
(define ((norm-cdf* Ds) pi)
  (define N (count-configurations pi (const #t)))
  (define overheads
    (sort (for/list ([cfg (in-configurations pi)])
            (overhead pi (configuration-info->mean-runtime cfg)))
          <))

  ;; Calculate total counts
  (define D+k*
    (let go ([k 0]
             [Ds Ds]
             [overheads overheads]
             [result (hash)])
      (cond
        ;; Domain is finished, we're done.
        [(stream-empty? Ds) result]
        ;; All overheads must be under remaining Ds.
        [(stream-empty? overheads)
         (define result* (hash-set result (stream-first Ds) k))
         (go k (stream-rest Ds) overheads result*)]
        ;; Flip-flop between recursion on D and overheads.
        [else
         (define-values (D overhead)
           (values (stream-first Ds) (stream-first overheads)))
         (if (<= overhead D)
             (go (add1 k) Ds (stream-rest overheads) result)
             (go k (stream-rest Ds) overheads (hash-set result D k)))])))

  ;; Normalize by N
  (for/hash ([(D k) (in-hash D+k*)])
    (values D (/ k N))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lattice figures

;;
;; TODO
(define (make-lattices baseline-pis opt-pis benchmarks [spacing 40])
  (define lattice-picts
    (for/list ([baseline-pi (in-list baseline-pis)]
               [opt-pi (in-list opt-pis)]
               #:when (member (performance-info->name baseline-pi)
                              benchmarks))
      (make-performance-lattice (pi->vector baseline-pi) (pi->vector opt-pi))))
  (apply hc-append spacing lattice-picts))

;;
;; TODO
(define (pi->vector pi)
  (define h
    (for/hash ([cfg (in-configurations pi)])
      (values (configuration-info->id cfg) cfg)))
  (define units (performance-info->num-units pi))
  (define total-cfgs (expt 2 units))
  (define sorted-cfgs
    (for/list ([k (in-range (sub1 total-cfgs) -1 -1)])
      (hash-ref h (natural->bitstring k #:bits units))))
  (for/vector ([cfg (in-list sorted-cfgs)])
    (define runtimes (configuration-info->runtime* cfg))
    (cons (mean runtimes) (stddev runtimes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grid figures

(define ((make-grid-figure plot) . pis*)
  (define pis-grouped (apply map list pis*))
  (grid-plot plot pis-grouped))

(define make-overhead-grid (make-grid-figure overhead-plot))
(define make-exact-grid (make-grid-figure exact-runtime-plot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table

;; [Parameter Real]
;; TODO
(define *LATTICE-RED-THRESHOLD* (make-parameter 3))

;; [Parameter Real]
;; TODO
(define *LATTICE-GREEN-THRESHOLD* (make-parameter 1.25))

;;
;;
(define HEADER0
  `(""
    "Racket Overhead" cont
    ,(elem scv-cr " Overhead") cont
    ,(elem scv-cr " Analyze")
    ,(elem scv-cr " Compile")))

;;
;;
(define HEADER1
  '("Benchmark"
    "Max" "Mean"
    "Max" "Mean"
    "Mean ± 𝜎 (s)"
    "Mean ± 𝜎 (s)"))

;;
;;
(define FORMATTERS
  (list format-benchmark
        format-overhead
        format-overhead
        format-overhead
        format-overhead
        format-interval
        format-interval))

;;
;; TODO
(define (make-table-summary baseline-pis opt-pis analyses)
  (define data
    (for/list ([baseline-pi (in-list baseline-pis)]
               [opt-pi (in-list opt-pis)]
               [analysis (in-list analyses)])
      (define as (analyze-times analysis))
      (define cs (compile-times analysis))
      (applies FORMATTERS
               (list (performance-info->name baseline-pi)
                     (max-overhead baseline-pi)
                     (mean-overhead baseline-pi)
                     (max-overhead opt-pi)
                     (mean-overhead opt-pi)
                     (cons (mean as) (stddev as))
                     (cons (mean cs) (stddev cs))))))
  (tabular
   #:style 'boxed
   #:row-properties '(center)
   (cons HEADER0 (cons HEADER1 data))))

;;
;; TODO
(define (analyze-times analysis)
  (for/list ([h (in-list analysis)])
    (hash-ref h 'analyze-real)))

;;
;; TODO
(define (compile-times analysis)
  (for/list ([h (in-list analysis)])
    (+ (hash-ref h 'compile-real)
       (hash-ref h 'expand-real))))

;;
;; TODO
(define (applies fs xs)
  (for/list ([f (in-list fs)]
             [x (in-list xs)])
    (f x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; figures

(define overhead-summary (make-overhead-summary BASELINE-PIS OPT-PIS))
(define lattices (make-lattices BASELINE-PIS OPT-PIS '(sieve zombie)))
(define overhead-grid (make-overhead-grid BASELINE-PIS OPT-PIS))
(define exact-grid (make-exact-grid BASELINE-PIS OPT-PIS))
(define table-summary (make-table-summary BASELINE-PIS OPT-PIS ANALYSES))
