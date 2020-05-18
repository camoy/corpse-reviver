#lang typed/racket/base

;; Create L-N/M plots for .rktd files

;; Input:
;; - Raw experimental data (.rktd)
;;   (Optionally a list)
;; Output:
;; - Pict showing an L-N/M plot for the data
;;   (Or, a list of such plots)

(provide
  lnm-plot)

;; -----------------------------------------------------------------------------

(require
  corpse-reviver/require-typed-check
  "summary-adapted.rkt"
  "plot-adapted.rkt"
  (only-in racket/math exact-floor)
  (only-in racket/math exact-floor exact-ceiling))

(require/typed racket/stream
  [stream-length (-> (Sequenceof String) Index)]
  [stream->list (-> (Sequenceof String) (Listof String))]
  [stream-filter (-> (-> String Boolean) (Sequenceof String) (Sequenceof String))]
)
(require/typed/check "bitstring.rkt"
  [in-reach (-> String Index (Listof String))]
  [log2 (-> Index Index)]
)

;; =============================================================================
;; --- constants

(define DEFAULT_N 3)
(define DEFAULT_M 10)
(define DEFAULT_XLIMIT 20)
(define DEFAULT_CUTOFF 0.6)
(define DEFAULT_SAMPLES 20)

(define THIN (* 1.2 (line-width)))
(define THICK (* 1.8 (line-width)))

;; -----------------------------------------------------------------------------
;; --- plotting

(: lnm-plot (->* [Summary
                  (U Index (Listof Index))]
                 [Index
                  Index
                  Index
                  Positive-Integer
                  Real
                  Positive-Integer
                  Positive-Integer
                  ]
                  (Listof Pict)))
(define (lnm-plot summary
                  L ;; (U Index (Listof Index)), L-values to plot
                  [N DEFAULT_N]  ;; Index, recommened N limit
                  [M DEFAULT_M] ;; Index, recommended M limit
                  [xmax DEFAULT_XLIMIT] ;; Index, max. x-value
                  [num-samples DEFAULT_SAMPLES] ;; Index
                  [cutoff-proportion DEFAULT_CUTOFF] ;; Flonum, between 0 and 1.
                  [width (plot-width)] ;; Index
                  [height (plot-height)]) ;; Index
  (define L-list (or (and (list? L) L) (list L)))
  (define num-vars (get-num-variations summary))
  (define cutoff-point (* cutoff-proportion num-vars))
  ;; Make renderers for the lines
  (define N-line (vertical-line N num-vars
                                  'forestgreen
                                  THIN))
  (define M-line (vertical-line M num-vars
                                  'goldenrod
                                  THIN))
  (define cutoff-line (horizontal-line cutoff-point xmax
                                                    'orangered
                                                    'short-dash
                                                    THICK))
  ;; Get yticks
  (define yticks (compute-yticks num-vars 6 (list cutoff-point)))
  ;; Set plot parameters ('globally', for all picts)
  (plot-x-ticks (compute-xticks 5))
  (plot-y-ticks (compute-yticks num-vars 6 cutoff-point))
  (plot-x-far-ticks no-ticks)
  (plot-y-far-ticks no-ticks)
  (plot-font-face "bold")
  (plot-font-size 16)
  ;; Create 1 pict for each value of L
  (for/list ([L (in-list L-list)])
    (define F (function (count-variations summary L xmax) 0 xmax
                        num-samples
                        'navy
                        THICK))
    (define res (plot-pict (list N-line M-line cutoff-line F)
                           0 xmax
                           0 num-vars
                           "Overhead (vs. untyped)" "Count"
                           width height))
    (if (pict? res) res (error 'lnm))))

;; Return a function (-> Real Index) on argument `N`
;;  that counts the number of variations
;;  which can reach, in L or fewer steps,
;;  a variation with overhead no more than `N`
(: count-variations (->* [Summary Index] [(U #f Index)] (-> Real Natural)))
(define (count-variations sm L [lim #f])
  (define baseline (untyped-mean sm))
  (define cache (and lim (cache-init sm lim L)))
  (lambda ([N-raw : Real])
    (: N Nonnegative-Real)
    (define N (if (>= N-raw 0) N-raw (error 'count-variations)))
    (: good? (-> String Boolean))
    (define good? (make-variation->good? sm (* N baseline) L))
    (if (and cache lim (<= N lim))
        ;; Use cache to save some work, only test the variations
        ;; in the next bucket
        (cache-lookup cache N good?)
        ;; No cache, need to test all variations
        (stream-length (predicate->variations sm good?)))))

;; Make a predicate checking whether a variation is good.
;; Good = no more than `L` steps away from a variation
;;        with average runtime less than `good-threshold`.
(: make-variation->good? (->* [Summary Real] [Index] (-> String Boolean)))
(define (make-variation->good? summary good-threshold [L 0])
  (lambda ([var : String])
    (for/or ([var2 (cons var (in-reach var L))])
      (< (variation->mean-runtime summary var2)
         good-threshold))))

;; -----------------------------------------------------------------------------
;; -- cache

;; Caches map index-valued overheads to a list of variations
;; - cache entry 0 has no variations (or, all vars with less than 0x untyped runtime)
;; - cache entry 1 has all variations with overhead less than 1x untyped
;; - cache entry 2 has all variations with overhead between 1x (inclusive) and 2x (exc)
;; And so on.
(define-type Cache (Vectorof (Listof String)))

;; Create a cache that saves the configurations between discrete overhead values
(: cache-init (->* [Summary Index] [Index] Cache))
(define (cache-init summary max-overhead [L 0])
  (define base-overhead (untyped-mean summary))
  (: unsorted-variations (Boxof (Sequenceof String)))
  (define unsorted-variations (box (all-variations summary)))
  ;; For each integer-overhead-range [0, 1] [1, 2] ... [max-1, max]
  ;; save the variations within that overhead to a cache entry
  (for/vector : Cache
              ([i (in-range (add1 max-overhead))])
    (: good? (-> String Boolean))
    (define good? (make-variation->good? summary (* i base-overhead) L))
    (define-values (good-vars rest)
      (stream-partition good? (unbox unsorted-variations)))
    (set-box! unsorted-variations rest)
    (stream->list good-vars)))

;; Count the number of variations with running time less than `overhead`.
;; Use `test-fun` to manually check variations we aren't sure about
(: cache-lookup (-> Cache Nonnegative-Real (-> String Boolean) Natural))
(define (cache-lookup $$$ overhead test-fun)
  (: lo-overhead Natural)
  (define lo-overhead (exact-floor overhead))
  (: hi-overhead Natural)
  (define hi-overhead (exact-ceiling overhead))
  (define num-known
    (for/sum : Natural ([i (in-range (add1 lo-overhead))])
      (length (vector-ref $$$ i))))
  (if (= hi-overhead lo-overhead)
      ;; Short circuit, because original overhead was an integer
      num-known
      ;; Else test all the variations in the "next" bucket
      (+ num-known
         (for/sum : Natural ([var (in-list (vector-ref $$$ hi-overhead))]
                   #:when (test-fun var)) 1))))

(: stream-partition (-> (-> String Boolean) (Sequenceof String) (Values (Sequenceof String) (Sequenceof String))))
(define (stream-partition f stream)
  (define not-f (lambda ([x : String]) (not (f x))))
  (values (stream-filter f stream)
          (stream-filter not-f stream)))

;; -----------------------------------------------------------------------------
;; --- plotting utils

;; Compute `num-ticks` evenly-spaced y ticks between 0 and `max-y`.
;; Round all numbers down a little, except for numbers in the optional
;;  list `exact`.
(: compute-yticks (->* [Index Index] [(U Real (Listof Real))] ticks))
(define (compute-yticks max-y num-ticks [exact '()])
  (define exact-list (or (and (list? exact) exact) (list exact)))
  (define round-y (if (< max-y 1000)
                      round
                      (lambda ([n : Real]) (* 100 (exact-floor (/ n 100))))))
  (ticks (lambda ([ax-min : Real] [ax-max : Real])
           (for/list : (Listof pre-tick)
                     ([y (in-list (linear-seq ax-min ax-max num-ticks))])
             (define rounded (round-y y))
             (define ex (findf (lambda ([n : Real]) (= rounded (round-y n)))
                               exact-list))
             (pre-tick (or (and ex (round ex))
                           rounded)
                       #t)))
         (lambda ([ax-min : Real] [ax-max : Real] [pre-ticks : (Listof pre-tick)])
                 (for/list : (Listof String) ([pt (in-list pre-ticks)])
                   (number->string (pre-tick-value pt))))))

(: compute-xticks (-> Index ticks))
(define (compute-xticks num-ticks)
  (ticks (lambda ([ax-min : Real] [ax-max : Real])
           (for/list : (Listof pre-tick)
                     ([i (in-list (linear-seq 1 ax-max num-ticks))])
             (pre-tick (round i) #t)))
         (lambda ([ax-min : Real] [ax-max : Real] [pre-ticks : (Listof pre-tick)])
           (for/list : (Listof String) ([pt (in-list pre-ticks)])
             (format "~ax" (pre-tick-value pt))))))

(: horizontal-line (->* [Real]
                        [Index
                         Symbol
                         Plot-Pen-Style
                         Nonnegative-Real]
                        renderer2d))
(define (horizontal-line y-val
                         [x-max 1]
                         [c 'black]
                         [s 'solid]
                         [w (line-width)])
  (lines (list (list 0 y-val)
               (list x-max y-val))
         c
         w
         s))

(: vertical-line (->* [Real]
                      [Index
                       Symbol
                       Nonnegative-Real
                       Plot-Pen-Style]
                      renderer2d))
(define (vertical-line x-val
                       [y-max 1]
                       [c 'black]
                       [w (line-width)]
                       [s 'solid])
  (lines (list (list x-val 0)
               (list x-val y-max))
         c
         w
         s))
