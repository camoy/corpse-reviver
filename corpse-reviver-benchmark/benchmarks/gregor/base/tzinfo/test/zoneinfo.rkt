#lang racket/base

(require racket/date
         racket/runtime-path
         rackunit)
(require "../main.rkt"
         "../zoneinfo.rkt")

(define-runtime-path TEST64-DIR "data/64-bit")
(define-runtime-path TEST32-DIR "data/32-bit")

(define IN-DST 1409606993)
(define GAP-START (find-seconds 0 0 2 9 3 2014 #f))
(define OVERLAP-START (find-seconds 0 0 1 2 11 2014 #f))

(parameterize* ([current-zoneinfo-search-path (list "/path/to/nowhere"
                                                    TEST64-DIR)]
                [current-tzinfo-source (make-zoneinfo-source)])
  
  ;; LMT data for this zone is only available in the 64-bit files
  (check-equal? (utc-seconds->tzoffset "US/Eastern" -999999999999999)
                (tzoffset -17762 #f "LMT"))

  (check-equal? (utc-seconds->tzoffset "US/Eastern" IN-DST)
                (tzoffset -14400 #t "EDT"))
  
  (check-equal? (local-seconds->tzoffset "US/Eastern" IN-DST)
                (tzoffset -14400 #t "EDT"))
  
  (check-equal? (local-seconds->tzoffset "US/Eastern" GAP-START)
                (tzgap (+ GAP-START 18000)
                       (tzoffset -18000 #f "EST")
                       (tzoffset -14400 #t "EDT")))
  
  (check-equal? (local-seconds->tzoffset "US/Eastern" (sub1 GAP-START))
                (tzoffset -18000 #f "EST"))
  
  (check-equal? (local-seconds->tzoffset "US/Eastern" OVERLAP-START)
                (tzoverlap (tzoffset -14400 #t "EDT")
                           (tzoffset -18000 #f "EST")))
  
  (check-equal? (local-seconds->tzoffset "US/Eastern" (sub1 OVERLAP-START))
                (tzoffset -14400 #t "EDT"))
  
  (check-equal? (local-seconds->tzoffset "US/Eastern" (+ OVERLAP-START 3600))
                (tzoffset -18000 #f "EST"))
  
  (check-equal? (local-seconds->tzoffset "US/Eastern" (sub1 (+ OVERLAP-START 3600)))
                (tzoverlap (tzoffset -14400 #t "EDT")
                           (tzoffset -18000 #f "EST"))))

(parameterize* ([current-zoneinfo-search-path (list "/path/to/nowhere"
                                                    TEST32-DIR)]
                [current-tzinfo-source (make-zoneinfo-source)])
  ;; LMT data for this zone is only available in the 64-bit files
  (check-equal? (utc-seconds->tzoffset "US/Eastern" -999999999999999)
                (tzoffset -18000 #f "EST"))
  
  (check-equal? (utc-seconds->tzoffset "UTC" 238635325)
                (tzoffset 0 #f "UTC"))
  
  (check-exn exn:fail:tzinfo:zone-not-found?
             (λ ()
               (utc-seconds->tzoffset "Fillory/Whitespire" 675765756))))
