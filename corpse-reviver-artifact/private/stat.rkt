#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide median-baseline
         mean-opt
         max-baseline
         max-opt
         %-2x-baseline
         95%-quantile-opt
         %-baseline-within-max-opt
         sieve-large
         sieve-small
         morsecode-max
         zombie-mean
         7%-baseline
         7%-opt
         baseline-version)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require gtp-plot/configuration-info
         gtp-plot/performance-info
         gtp-plot/sample-info
         math/base
         math/distributions
         math/statistics
         mischief/for
         racket/format
         racket/list
         racket/match
         racket/runtime-path
         "read.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; Procedure → ([Listof Performance-Info] Any ... → [Listof Performance-Info])
;; Constructs a function that calculates weighted statistics over all benchmark
;; overheads.
(define ((make-overhead f) pis . rest)
  (cond
    [(null? pis) 0]
    [else
     (define-values (xs ws)
       (pis-overheads pis))
     (apply f xs ws rest)]))

;; [Listof Performance-Info] → Real
;; Returns the maximum overhead of all configurations.
(define max-overhead
  (make-overhead (λ (xs _) (apply max xs))))

;; [Listof Performance-Info] → Real
;; Returns the weighted mean overhead of all configurations.
(define mean-overhead (make-overhead mean))

;; [Listof Performance-Info] → Real
;; Returns the weighted median overhead of all configurations.
(define median-overhead
  (make-overhead (λ (xs ws) (median < xs ws))))

;; Real [Listof Performance-Info] → Real
;; Returns the weighted percent of configurations within a given overhead.
(define %-overhead<=
  (make-overhead
   (λ (xs ws x)
     (mc-probability (λ (x*) (<= x* x))
                     xs ws))))

;; [Listof Performance-Info] → Real
;; Returns the weighted quantile overhead of all configurations.
(define quantile-overhead
  (make-overhead (λ (xs ws q) (quantile q < xs ws))))

;; [Listof Performance-Info] → [Listof Real] [Listof Real]
;; Given a list of performance infos, returns the overhead over all
;; configurations and their weights to count all benchmarks equally.
(define (pis-overheads pis)
  (for/fold ([xs null]
             [ws null])
            ([pi (in-list pis)])
    (define/for/fold ([n 0]
                      [pi-xs null])
                     ([k (in-naturals 1)]
                      [ci (in-configurations pi)])
      (values k (cons (ci-overhead ci pi) pi-xs)))
    (values (append xs (reverse pi-xs))
            (append ws (make-list n (/ 1 n))))))

;; Performance-Info String → Real
;; Returns the overhead of a specific configuration in a performance info.
(define (configuration-overhead pi cfg-id)
  (for/first ([ci (in-configurations pi)]
              #:when (equal? (configuration-info->id ci) cfg-id))
    (ci-overhead ci pi)))

;; Configuration-Info Performance-Info → Real
;; Returns the overhead for a given configuration info.
(define (ci-overhead ci pi)
  (define baseline (performance-info->baseline-runtime pi))
  (define x (configuration-info->mean-runtime ci))
  (/ x baseline))

;; Symbol [Listof Performance-Info] → Performance-Info
;; Returns the first performance info that matches the given benchmark name.
(define (benchmark b pis)
  (for/first ([pi (in-list pis)]
              #:when (equal? (performance-info->name pi) b))
    pi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; statistics

(define-syntax-rule (and* ?x ...)
  (let ([result (and ?x ...)])
    (if result
        result
        0)))

(define median-baseline
  (format-overhead (median-overhead BASELINE-PIS)))

(define mean-opt
  (format-overhead (mean-overhead OPT-PIS)))

(define max-baseline
  (format-overhead (max-overhead BASELINE-PIS)))

(define max-opt
  (format-overhead (max-overhead OPT-PIS)))

(define %-2x-baseline
  (format-percent (%-overhead<= BASELINE-PIS 2)))

(define 95%-quantile-opt
  (format-overhead (quantile-overhead OPT-PIS 0.95)))

(define %-baseline-within-max-opt
  (format-percent (%-overhead<= BASELINE-PIS (max-overhead OPT-PIS))))

(define sieve-pi (benchmark 'sieve BASELINE-PIS))

(define sieve-01
  (and* sieve-pi (configuration-overhead sieve-pi "01")))

(define sieve-10
  (and* sieve-pi (configuration-overhead sieve-pi "10")))

(define sieve-large
  (and* sieve-01 sieve-10 (format-overhead (max sieve-01 sieve-10))))

(define sieve-small
  (and* sieve-01 sieve-10 (format-overhead (min sieve-01 sieve-10))))

(define morsecode-pi (benchmark 'morsecode BASELINE-PIS))

(define morsecode-max
  (and* morsecode-pi (format-overhead (max-overhead (list morsecode-pi)))))

(define zombie-pi (benchmark 'zombie BASELINE-PIS))

(define zombie-mean
  (and* zombie-pi (format-overhead (max-overhead (list zombie-pi)))))

(define 7%-baseline
  (format-percent (%-overhead<= BASELINE-PIS 1.07)))

(define 7%-opt
  (format-percent (%-overhead<= OPT-PIS 1.07)))

(define baseline-version (or BASELINE-VERSION "???"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; output tex

(module+ main
  (define STATS
    `(("medianBaseline" ,median-baseline)
      ("meanOpt" ,mean-opt)
      ("maxBaseline" ,max-baseline)
      ("maxOpt" ,max-opt)
      ("twoTimesBaseline" ,%-2x-baseline)
      ("nineFiveQuantileOpt" ,95%-quantile-opt)
      ("baselineWithinMaxOpt" ,%-baseline-within-max-opt)
      ("sieveLarge" ,sieve-large)
      ("sieveSmall" ,sieve-small)
      ("morsecodeMax" ,morsecode-max)
      ("zombieMean" ,zombie-mean)
      ("sevenPctBaseline" ,7%-baseline)
      ("sevenPctOpt" ,7%-opt)
      ("baselineVersion" ,baseline-version)))

  (displayln "% Output from `corpse-reviver-artifact/private/stat.rkt`")
  (for ([stat (in-list STATS)])
    (match-define (list name val) stat)
    (define val* (regexp-replace #rx"%" (format "~a" val) "\\\\%"))
    (define val** (regexp-replace #rx"×" (format "~a" val*) "\\\\times"))
    (displayln (format "\\newcommand\\~a{$~a$\\xspace}" name val**))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk)

  (define (c x y z)
    (configuration-info x y (list z)))

  (define pi-2-units
    (make-performance-info
     '2-units
     #:src "."
     #:num-units 2
     #:num-configurations 4
     #:baseline-runtime* '(2)
     #:untyped-runtime* '(2)
     #:typed-runtime* '(1)
     #:make-in-configurations
     (λ _
       (list (c "00" 0 2)
             (c "01" 1 6)
             (c "10" 1 10)
             (c "11" 2 1)))))

  (define pi-3-units
    (make-performance-info
     '3-units
     #:src "."
     #:num-units 3
     #:num-configurations 8
     #:baseline-runtime* '(1)
     #:untyped-runtime* '(1)
     #:typed-runtime* '(0.9)
     #:make-in-configurations
     (λ _
       (list (c "000" 0 1)
             (c "001" 1 5)
             (c "010" 1 6)
             (c "011" 2 8)
             (c "100" 1 10)
             (c "101" 2 6)
             (c "110" 2 2)
             (c "111" 3 1)))))

  (define si-3-units
    (make-sample-info
     pi-3-units
     (list (list (c "001" 1 5)
                 (c "010" 1 6))
           (list (c "101" 2 6)
                 (c "110" 2 2)))))

  (define ALL (list pi-2-units pi-3-units si-3-units))

  (with-chk (['name "max-overhead"])
    (chk
     (max-overhead (list pi-2-units pi-3-units si-3-units))
     10))

  (with-chk (['name "mean-overhead"])
    (chk
     (mean-overhead (list pi-2-units pi-3-units si-3-units))
     4))

  (with-chk (['name "median-overhead"])
    (chk
     (median-overhead (list pi-2-units pi-3-units si-3-units))
     5))

  (with-chk (['name "%-overhead<="])
    (chk
     (%-overhead<= (list pi-2-units pi-3-units si-3-units) 4)
     11/24))

  (with-chk (['name "quantile-overhead"])
    (chk
     (quantile-overhead (list pi-2-units pi-3-units si-3-units) 1/2)
     5))

  (with-chk (['name "pis-overheads"])
    (chk
     (pis-overheads (list pi-2-units pi-3-units si-3-units))
     (values
      '(1 3 5 1/2 1 5 6 8 10 6 2 1 5 6 6 2)
      '(1/4 1/4 1/4 1/4 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/4 1/4 1/4 1/4))))

  (with-chk (['name "configuration-overhead"])
    (chk
     (configuration-overhead pi-2-units "01")
     3
     (configuration-overhead pi-3-units "011")
     8))

  (with-chk (['name "ci-overhead"])
    (chk
     (ci-overhead (c "010" 1 2) pi-3-units)
     2
     (ci-overhead (c "01" 1 12) pi-2-units)
     6))

  (with-chk (['name "benchmark"])
    (chk
     (benchmark '2-units ALL)
     pi-2-units
     (benchmark '3-units ALL)
     pi-3-units
     (benchmark '3-units (list si-3-units))
     si-3-units
     #:! #:t (benchmark '2-unitz ALL)))
  )
