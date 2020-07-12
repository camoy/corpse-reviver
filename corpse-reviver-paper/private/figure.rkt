#lang at-exp racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

#;(provide fig:overhead-summary
         fig:lattices
         fig:overhead-grid
         fig:exact-grid)

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
         threading
         "lattice.rkt"
         "read.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; consts

(define COLOR-SCHEME (list #xfdb863 #xb2abd2))
(define DARK-COLOR-SCHEME (list #xe66101 #x5e3c99))

(define-runtime-path BASELINE-DIR "../baseline")
(define-runtime-path OPT-DIR "../opt")

(define BASELINE-PIS (hash->sorted-list (dir->pi-hash BASELINE-DIR)))
(define OPT-PIS (hash->sorted-list (dir->pi-hash OPT-DIR)))

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
;; grid figures

(define ((make-grid-figure plot) . pis*)
  (define pis-grouped (apply map list pis*))
  (grid-plot plot pis-grouped))

(define overhead-grid (make-grid-figure overhead-plot))
(define exact-grid (make-grid-figure exact-runtime-plot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; summary figure

;; [Parameterof Real]
;;
(define *SUMMARY-GRANULARITY* (make-parameter 1/100))

;;
;; TODO
(define (overhead-summary . pis*)
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

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table

(define *LATTICE-RED-THRESHOLD* (make-parameter 3))
(define *LATTICE-GREEN-THRESHOLD* (make-parameter 1.25))

(define (color x)
  (if (number? x)
      (let ([x* (real->decimal-string x 1)])
        (cond
          [(<= x (*LATTICE-GREEN-THRESHOLD*))
           @~a{\cellcolor{rktpalegreen} @x*}]
          [(>= x (*LATTICE-RED-THRESHOLD*))
           @~a{\cellcolor{rktpink} @x*}]
          [else
           x*]))
      x))

;;          (hash-ref (make-paths->hash "runtime" eg-all) "sieve")

(define (approx x)
  (~r (/ x 1000) #:precision 0))

(define (summary-template results)
  (define results*
    (string-join (map (λ (x)
                        (string-join (append (map color (take x 5)) (drop x 5))
                                     " & "))
                      results)
                 "\\\\"))
  @~a{\begin{tabular}{ c | c c | c c | c | c}
  & \multicolumn{2}{c|}{Racket Overhead}
  & \multicolumn{2}{c|}{\tool Overhead}
  & \multicolumn{1}{c}{\tool Analyze}
  & \multicolumn{1}{c}{\tool Compile} \\
  Benchmark
  & \hspace{0.65em}Max\hspace{0.65em} & Mean
  & \hspace{0.65em}Max\hspace{0.65em} & Mean
  & \hspace{0.65em}Mean $\pm~\sigma$ (s)
  & \hspace{0.65em}Mean $\pm~\sigma$ (s)  \\
  \hline
  @results* \\
  \end{tabular}})

(define (summary-table baseline opt)
  (match-define (list baseline-pis opt-pis)
    (datasets->pis (λ (pi benchmark _) (cons benchmark pi))
                   (list baseline opt)))
  (define analysis-hashes (opt->analyses opt))
  (with-output-to-file "summary.tex"
    #:exists 'replace
    (λ ()
      (displayln
       (summary-template
        (for/list ([baseline-pi (in-list baseline-pis)]
                   [opt-pi (in-list opt-pis)]
                   [analysis (in-list analysis-hashes)])
          (define-values (analysis-totals compile-totals)
            (for/fold ([a null] [b null])
                      ([h (in-list analysis)])
              (values (cons (hash-ref h 'analyze-real) a)
                      (cons (+ (hash-ref h 'compile-real)
                               (hash-ref h 'expand-real))
                            b))))
          (define benchmark (car baseline-pi))
          (list (format "\\textsc{~a}" benchmark)
                (max-overhead (cdr baseline-pi))
                (mean-overhead (cdr baseline-pi))
                (max-overhead (cdr opt-pi))
                (mean-overhead (cdr opt-pi))
                (format "~a $\\pm$ ~a"
                        (approx (mean analysis-totals))
                        (approx (stddev analysis-totals)))
                (format "~a $\\pm$ ~a"
                        (approx (mean compile-totals))
                        (approx (stddev compile-totals)))))))))
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lattices baseline opt benchmarks [spacing 40])
  (define-values (baseline* opt*)
    (values (filter-benchmark baseline benchmarks)
            (filter-benchmark opt benchmarks)))
  (apply hc-append spacing (lattices-list baseline* opt*)))

(define (lattices-list baseline opt)
  (match-define (list baseline-pis opt-pis)
    (datasets->pis (λ (pi . _) pi)
                   (list baseline opt)))
  (for/list ([baseline-pi (in-list baseline-pis)]
             [opt-pi (in-list opt-pis)])
    (define height (performance-info->num-units baseline-pi))
    (make-performance-lattice (average-results baseline-pi)
                              (average-results opt-pi))))

(define (average-results pi)
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
    (cons (mean runtimes)
          (stddev runtimes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; figures

(define fig:overhead-summary
  (overhead-summary BASELINE OPT))

(define fig:lattices
  (lattices BASELINE OPT '("sieve" "zombie")))

(define fig:overhead-grid
  (overhead-grid BASELINE OPT))

(define fig:exact-grid
  (exact-grid BASELINE OPT))

(define table:summary
  (summary-table BASELINE OPT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; figures

(define fig:overhead-summary (overhead-summary BASELINE-PIS OPT-PIS))
(define fig:overhead-grid (overhead-grid BASELINE-PIS OPT-PIS))
(define fig:exact-grid (exact-grid BASELINE-PIS OPT-PIS))
