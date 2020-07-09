#lang at-exp racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide fig:overhead-summary
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
         racket/string
         racket/hash
         racket/function
         racket/format
         racket/path
         racket/list
         racket/runtime-path
         racket/match
         racket/set
         rebellion/collection/multidict
         rebellion/collection/entry
         threading
         "lattice.rkt"
         "read.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; const


(define COLOR-SCHEME (list #xfdb863 #xb2abd2))
(define DARK-COLOR-SCHEME (list #xe66101 #x5e3c99))

(*SAMPLE-INTERVAL-ALPHA* 0.6)
(*MULTI-INTERVAL-ALPHA* 0.6)
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

(define ((color-converter scheme) k)
  (define k* (modulo k (length scheme)))
  (hex-triplet->color% (list-ref scheme k*)))

(*BRUSH-COLOR-CONVERTER* (color-converter COLOR-SCHEME))
(*PEN-COLOR-CONVERTER* (color-converter DARK-COLOR-SCHEME))

(define-runtime-path BASELINE-PATH "../baseline")
(define BASELINE (directory-list BASELINE-PATH #:build? BASELINE-PATH))

(define-runtime-path OPT-PATH "../opt")
(define OPT (directory-list OPT-PATH #:build? OPT-PATH))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public

(define ((make-grid-figure pi-fun plot) . datasets)
  (define pis*
    (apply map list (datasets->pis pi-fun datasets)))
  (grid-plot plot pis*))

(define (overhead-or-samples pis)
  (if (sample-info? (first pis))
      (samples-plot pis)
      (overhead-plot pis)))

;;
;;
(define overhead-grid
  (make-grid-figure
   (λ (pi benchmark paths)
     (if (exhaustive? pi) pi (runtime-paths->sample-info pi paths)))
   overhead-or-samples))

;;
;;
(define exact-grid (make-grid-figure (λ (pi . _) pi) exact-runtime-plot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; summary CDF

;; TODO: this is slower than it should be
(define *SUMMARY-GRANULARITY* (make-parameter 1/100))

(define (overhead-summary . datasets)
  (define pis*
    (datasets->pis (λ (x . _) x) datasets))
  (define pis**
    (for/list ([pis (in-list pis*)])
      (define points (slowdown-points pis))
      (make-performance-info
       '||
       #:src "."
       #:num-units 0
       #:num-configurations (length points)
       #:baseline-runtime* '(1)
       #:untyped-runtime* '(1)
       #:typed-runtime* '(1)
       #:make-in-configurations
       (λ _ points))))
  (parameterize ([*OVERHEAD-SHOW-CONFIGURATIONS* #f])
    (overhead-plot pis**)))

(define (slowdown-points pis)
  (define interval
    (in-range 1 (*OVERHEAD-MAX*) (*SUMMARY-GRANULARITY*)))
  (define size (/ (sub1 (*OVERHEAD-MAX*)) (*SUMMARY-GRANULARITY*)))
  (define inv (adjusted-inverse-cdf pis interval))
  (for/list ([ind (in-naturals 1)]
             [k interval])
    (define p (/ ind size))
    (configuration-info "" 0 (list (inv p)))))

(define (adjusted-inverse-cdf pis x-vals)
  (define graph
    (for/list ([xy (in-list (adjusted-cdf pis x-vals))])
      (cons (cdr xy) (car xy))))
  (λ (x)
    (define p (assf (λ (v) (>= v x)) graph))
    (if p (cdr p) 100)))

(define (adjusted-cdf pis x-vals)
  (for/list ([k x-vals])
    (cons k
          (/ (for/sum ([pi (in-list pis)])
               (define n (count-configurations pi (const #t)))
               (percent-k-deliverable k n pi))
             (length pis)))))

(define (percent-k-deliverable k n pi)
  (/ ((deliverable k) pi) n))

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

(define (summary-template results)
  (define results*
    (string-join (map (λ (x)
                        (string-join (map color x) " & "))
                      results)
                 "\\\\"))
  @~a{\begin{tabular}{ c | c c | c c }
  & \multicolumn{2}{c|}{Racket Overhead}
  & \multicolumn{2}{c}{\tool Overhead} \\
  Benchmark
  & \hspace{0.65em}Max\hspace{0.65em} & Mean
  & \hspace{0.65em}Max\hspace{0.65em} & Mean \\
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
                   [opt-pi (in-list opt-pis)])
          (define benchmark (car baseline-pi))
          (list (format "\\textsc{~a}" benchmark)
                (max-overhead (cdr baseline-pi))
                (mean-overhead (cdr baseline-pi))
                (max-overhead (cdr opt-pi))
                (mean-overhead (cdr opt-pi))))))))
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
  #f #;(overhead-summary BASELINE OPT))

(define fig:lattices
  #f #;(lattices BASELINE OPT '("sieve" "zombie")))

(define fig:overhead-grid
  (overhead-grid BASELINE OPT))

(define fig:exact-grid
  (exact-grid BASELINE OPT))

(define table:summary
  (summary-table BASELINE OPT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

fig:overhead-grid
