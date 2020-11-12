#lang at-exp racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide overhead-summary
         lattices
         ut-require-ty
         ty-require-ut
         overhead-grid
         exact-grid
         table-summary

         orange-key
         purple-key
         light-orange-key
         light-blue-key
         dark-blue-key
         red-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/draw
         gtp-plot/configuration-info
         gtp-plot/performance-info
         gtp-plot/plot
         gtp-plot/sample-info
         (only-in gtp-util natural->bitstring)
         (only-in metapict save-pict)
         json
         math/statistics
         ppict/pict
         pict
         (except-in pict-abbrevs save-pict)
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
         "diagram.rkt"
         "read.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; consts

(define ORANGE #xfdb863)
(define PURPLE #xb2abd2)
(define DARK-ORANGE #xe66101)
(define DARK-PURPLE #x5e3c99)

(define COLOR-SCHEME (list ORANGE PURPLE))
(define DARK-COLOR-SCHEME (list DARK-ORANGE DARK-PURPLE))

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
    (for/list ([pis (in-list pis*)]
               #:when (not (null? pis)))
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
  (when (not (null? cdf-pis))
    (parameterize ([*OVERHEAD-SHOW-CONFIGURATIONS* #f]
                   [*OVERHEAD-PLOT-HEIGHT* (* 3/4 (*OVERHEAD-PLOT-HEIGHT*))]
                   [*FONT-SIZE* (ceiling (* 3/5 (*FONT-SIZE*)))])
      (overhead-plot cdf-pis))))

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
               #:when (member (performance-info->name baseline-pi) benchmarks)
               #:when (and (exhaustive? baseline-pi) (exhaustive? opt-pi)))
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
    ,(elem "SCV-CR Overhead") cont
    ,(elem "SCV-CR Analyze")
    ,(elem "SCV-CR Compile")))

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
(define TEX-HEADER
  @~a{
  \begin{tabular}{ c | c c | c c | c | c}
  & \multicolumn{2}{c|}{Racket Overhead}
  & \multicolumn{2}{c|}{\tool Overhead}
  & \multicolumn{1}{c|}{\tool Analyze}
  & \multicolumn{1}{c}{\tool Compile} \\
  Benchmark
  & \hspace{0.65em}Max\hspace{0.65em} & Mean
  & \hspace{0.65em}Max\hspace{0.65em} & Mean
  & \hspace{0.65em}Mean $\pm~\sigma$ (s)
  & \hspace{0.65em}Mean $\pm~\sigma$ (s)  \\
  \hline
  })

;;
;;
(define TEX-FOOTER
  @~a{\end{tabular}})

;;
;;
(define TEX-FORMATTERS
  (list format-benchmark-tex
        format-overhead-tex
        format-overhead-tex
        format-overhead-tex
        format-overhead-tex
        format-interval
        format-interval))

;;
;;
(define SCRIBBLE-FORMATTERS
  (list format-benchmark
        format-overhead
        format-overhead
        format-overhead
        format-overhead
        format-interval
        format-interval))

(define (make-table-data baseline-pis opt-pis analyses formatters)
  (for/list ([baseline-pi (in-list baseline-pis)]
             [opt-pi (in-list opt-pis)]
             [analysis (in-list analyses)])
    (define as (analyze-times analysis))
    (define cs (compile-times analysis))
    (applies formatters
             (list (performance-info->name baseline-pi)
                   (max-overhead baseline-pi)
                   (mean-overhead baseline-pi)
                   (max-overhead opt-pi)
                   (mean-overhead opt-pi)
                   (cons (mean as) (stddev as))
                   (cons (mean cs) (stddev cs))))))

;;
;; TODO
(define (make-table-summary baseline-pis opt-pis analyses)
  (define data
    (make-table-data baseline-pis opt-pis analyses SCRIBBLE-FORMATTERS))
  (tabular
   #:style 'boxed
   #:row-properties '(center)
   (cons HEADER0 (cons HEADER1 data))))

;;
;; TODO
(define (make-table-tex baseline-pis opt-pis analyses)
  (define data (make-table-data baseline-pis opt-pis analyses TEX-FORMATTERS))
  (define bodies
    (for/list ([row (in-list data)])
      (string-append (string-join row " & ") @~a{\\})))
  (string-append TEX-HEADER
                 (apply string-append bodies)
                 TEX-FOOTER))

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
;; keys

;; Number → Pict
;; TODO
(define (key color)
  (define color* (if (integer? color) (hex-triplet->color% color) color))
  (ppict-do
   (blank 15)
   #:go (coord 1/2 1/2 'cc)
   (filled-rounded-rectangle 13 13 #:color color* #:draw-border? #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; render

(define (save-ps path pict)
  (define dc
    (new post-script-dc%
         [width (pict-width pict)]
         [height (pict-height pict)]
         [interactive #f]
         [output path]))
  (send dc start-doc "")
  (send dc start-page)
  (draw-pict pict dc 0 0)
  (send dc end-page)
  (send dc end-doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; figures

(define overhead-summary (make-overhead-summary BASELINE-PIS OPT-PIS))
(define lattices (make-lattices BASELINE-PIS OPT-PIS '(sieve zombie)))
(define overhead-grid (make-overhead-grid BASELINE-PIS OPT-PIS))
(define exact-grid (make-exact-grid BASELINE-PIS OPT-PIS))
(define table-summary (make-table-summary BASELINE-PIS OPT-PIS ANALYSES))
(define table-tex (make-table-tex BASELINE-PIS OPT-PIS ANALYSES))

(define orange-key (key ORANGE))
(define purple-key (key PURPLE))
(define light-orange-key (key MOD-TY-COLOR))
(define light-blue-key (key MOD-UT-LINE))
(define dark-blue-key (key MOD-UT-TEXT))
(define red-key (key MOD-US-COLOR))

(module+ main
  (define exports
    `(("overhead-summary.pdf" . ,overhead-summary)
      ("lattices.pdf" . ,lattices)
      ("ut-require-ty.pdf" . ,ut-require-ty)
      ("ty-require-ut.pdf" . ,ty-require-ut)
      ("overhead-grid.pdf" . ,overhead-grid)
      ("exact-grid.pdf" . ,exact-grid)
      ("orange-key.pdf" . ,orange-key)
      ("purple-key.pdf" .  ,purple-key)
      ("light-orange-key.pdf" . ,light-orange-key)
      ("light-blue-key.pdf" . ,light-blue-key)
      ("dark-blue-key.pdf" . ,dark-blue-key)
      ("red-key.pdf" . ,red-key)))

  (for ([export (in-list exports)])
    (match-define (cons name pict) export)
    (save-pict name pict 'pdf))

  (with-output-to-file
    "summary.tex"
    (λ () (displayln table-tex))
    #:exists 'replace))
