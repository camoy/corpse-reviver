#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide stats:median-overhead-baseline
         stats:mean-overhead-opt
         stats:worst-case-baseline
         stats:worst-case-opt
         stats:percent-two-times-overhead-baseline
         stats:95-percentile-overhead-opt
         stats:percent-baseline-within-worst-case-opt
         stats:sieve-large-overhead
         stats:sieve-small-overhead
         stats:morsecode-max-overhead
         stats:zombie-mean-overhead
         stats:baseline-7%
         stats:opt-7%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/runtime-path
         racket/match
         racket/format
         mischief/for
         math/statistics
         gtp-plot/performance-info
         gtp-plot/configuration-info
         "read.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (worst-case pis)
  (apply max (map max-overhead pis)))

(define (mean-case pis)
  (mean (map mean-overhead pis)))

(define (median-case pis)
  (median < (pis-overheads pis)))

(define (percent-overhead<= pis target-overhead)
  (define count 0)
  (define total 0)
  (for ([pi (in-list pis)])
    (for ([ci (in-configurations pi)])
      (set! total (add1 total))
      (when (<= (ci-overhead ci pi) target-overhead)
        (set! count (add1 count)))))
  (* (/ count total) 100))

(define (overhead-percentile pis percentile)
  (define overheads (sort (pis-overheads pis) <))
  (define k (inexact->exact (ceiling (* percentile (length overheads)))))
  (list-ref overheads k))

(define (configuration-overhead pi cfg-id)
  (for/first ([ci (in-configurations pi)]
              #:when (equal? (configuration-info->id ci) cfg-id))
    (ci-overhead ci pi)))

(define (pis-overheads pis)
  (for/append ([pi (in-list pis)])
    (for/list ([ci (in-configurations pi)])
      (ci-overhead ci pi))))

(define (ci-overhead ci pi)
  (define baseline (performance-info->baseline-runtime pi))
  (define x (configuration-info->mean-runtime ci))
  (/ x baseline))

(define (benchmark pis b)
  (for/first ([pi (in-list pis)]
              #:when (equal? (performance-info->name pi) b))
    pi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define stats:median-overhead-baseline
  (format-overhead (median-case BASELINE-PIS)))

(define stats:mean-overhead-opt
  (format-overhead (mean-case OPT-PIS)))

(define stats:worst-case-baseline
  (format-overhead (worst-case BASELINE-PIS)))

(define stats:worst-case-opt
  (format-overhead (worst-case OPT-PIS)))

(define stats:percent-two-times-overhead-baseline
  (format-percent (percent-overhead<= BASELINE-PIS 2)))

(define stats:95-percentile-overhead-opt
  (format-overhead (overhead-percentile OPT-PIS 0.95)))

(define stats:percent-baseline-within-worst-case-opt
  (format-percent (percent-overhead<= BASELINE-PIS (worst-case OPT-PIS))))

(define sieve-01-overhead
  (configuration-overhead (benchmark BASELINE-PIS 'sieve) "01"))

(define sieve-10-overhead
  (configuration-overhead (benchmark BASELINE-PIS 'sieve) "10"))

(define stats:sieve-large-overhead
  (format-overhead (max sieve-01-overhead sieve-10-overhead)))

(define stats:sieve-small-overhead
  (format-overhead (min sieve-01-overhead sieve-10-overhead)))

(define stats:morsecode-max-overhead
  (format-overhead (max-overhead (benchmark BASELINE-PIS 'morsecode))))

(define stats:zombie-mean-overhead
  (format-overhead (max-overhead (benchmark BASELINE-PIS 'zombie))))

(define stats:baseline-7%
  (format-percent (percent-overhead<= BASELINE-PIS 1.07)))

(define stats:opt-7%
  (format-percent (percent-overhead<= OPT-PIS 1.07)))
