#lang racket/base

(provide stats:mean-overhead-baseline
         stats:mean-overhead-opt
         stats:worst-case-baseline
         stats:worst-case-opt
         stats:percent-two-times-overhead-baseline
         stats:95-percentile-overhead-opt
         stats:percent-baseline-within-worst-case-opt
         stats:sieve-large-overhead
         stats:sieve-small-overhead
         stats:morsecode-max-overhead
         stats:zombie-mean-overhead)

(require racket/runtime-path
         racket/match
         racket/format
         mischief/for
         math/statistics
         gtp-plot/performance-info
         gtp-plot/configuration-info
         "read.rkt")

;; DRY
(define-runtime-path BASELINE-PATH "../baseline")
(define BASELINE (directory-list BASELINE-PATH #:build? BASELINE-PATH))

(define-runtime-path OPT-PATH "../opt")
(define OPT (directory-list OPT-PATH #:build? OPT-PATH))

(match-define (list baseline-pis opt-pis)
  (datasets->pis (λ (pi benchmark _) (cons benchmark pi))
                 (list BASELINE OPT)))

(define (worst-case pis)
  (for/fold ([overhead -1])
            ([benchmark+pi (in-list pis)])
    (define pi (cdr benchmark+pi))
    (max overhead (max-overhead pi))))

(define MOST-PERCENT 0.8)

(define (mean-case pis)
  (define overheads
    (for/list ([benchmark+pi (in-list pis)])
      (define pi (cdr benchmark+pi))
      (mean-overhead pi)))
  (mean overheads))

(define (overhead pi cfg)
  (define baseline (performance-info->baseline-runtime pi))
  (define x (configuration-info->mean-runtime cfg))
  (/ x baseline))

(define (percent-overhead<= pis target-overhead)
  (define count 0)
  (define total 0)
  (for ([benchmark+pi (in-list pis)])
    (define pi (cdr benchmark+pi))
    (for ([cfg (in-configurations pi)])
      (set! total (add1 total))
      (when (<= (overhead pi cfg) target-overhead)
        (set! count (add1 count)))))
  (* (/ count total) 100))

(define (overhead-percentile pis percentile)
  (define overheads
    (for/append ([benchmark+pi (in-list pis)])
      (define pi (cdr benchmark+pi))
      (for/list ([cfg (in-configurations pi)])
        (overhead pi cfg))))
  (define overheads* (sort overheads <))
  (define k (inexact->exact (ceiling (* percentile (length overheads)))))
  (list-ref overheads* k))

(define (configuration-overhead pis benchmark cfg-id)
  (for/first ([benchmark+pi (in-list pis)]
              #:when (equal? (car benchmark+pi) benchmark))
    (define pi (cdr benchmark+pi))
    (for/first ([cfg (in-configurations pi)]
                #:when (equal? (configuration-info->id cfg) cfg-id))
      (overhead pi cfg))))

;; TODO dry

(define (benchmark-max-overhead pis benchmark)
  (for/first ([benchmark+pi (in-list pis)]
              #:when (equal? (car benchmark+pi) benchmark))
    (define pi (cdr benchmark+pi))
    (max-overhead pi)))

(define (benchmark-mean-overhead pis benchmark)
  (for/first ([benchmark+pi (in-list pis)]
              #:when (equal? (car benchmark+pi) benchmark))
    (define pi (cdr benchmark+pi))
    (mean-overhead pi)))

(define (fmt-overhead n) (string-append (~r n #:precision 1) "×"))
(define (fmt-percent n) (string-append (~r n #:precision 0) "%"))

(define stats:mean-overhead-baseline
  (fmt-overhead (mean-case baseline-pis)))

(define stats:mean-overhead-opt
  (fmt-overhead (mean-case opt-pis)))

(define stats:worst-case-baseline
  (fmt-overhead (worst-case baseline-pis)))

(define stats:worst-case-opt
  (fmt-overhead (worst-case opt-pis)))

(define stats:percent-two-times-overhead-baseline
  (fmt-percent (percent-overhead<= baseline-pis 2)))

(define stats:95-percentile-overhead-opt
  (fmt-overhead (overhead-percentile opt-pis 0.95)))

(define stats:percent-baseline-within-worst-case-opt
  (fmt-percent (percent-overhead<= baseline-pis (worst-case opt-pis))))

(define-values (sieve-01-overhead sieve-10-overhead)
  (values (configuration-overhead baseline-pis "sieve" "01")
          (configuration-overhead baseline-pis "sieve" "10")))

(define stats:sieve-large-overhead
  (fmt-overhead (max sieve-01-overhead sieve-10-overhead)))

(define stats:sieve-small-overhead
  (fmt-overhead (min sieve-01-overhead sieve-10-overhead)))

(define stats:morsecode-max-overhead
  (fmt-overhead (benchmark-max-overhead baseline-pis "morsecode")))

(define stats:zombie-mean-overhead
  (fmt-overhead (benchmark-mean-overhead baseline-pis "zombie")))
