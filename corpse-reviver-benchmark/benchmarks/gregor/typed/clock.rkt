#lang typed/racket/base

;; Front-end:
;; Working with current clock

(provide;/contract
 current-clock         ;any/c]
 current-posix-seconds ;any/c]
 now/moment            ;(->i () (#:tz [tz tz/c]) [res moment?])]
 now                   ;(->i () (#:tz [tz tz/c]) [res datetime?])]
 today                 ;(->i () (#:tz [tz tz/c]) [res date?])]
 current-time          ;(->i () (#:tz [tz tz/c]) [res time?])]
 now/moment/utc        ;(-> moment?)]
 now/utc               ;(-> datetime?)]
 today/utc             ;(-> date?)]
 current-time/utc      ;(-> time?)])
)

;; -----------------------------------------------------------------------------

(require
  corpse-reviver/require-typed-check
  "../base/types.rkt"
  "tzinfo-adapter.rkt"
  "gregor-adapter.rkt"
)
(require/typed/check "moment.rkt"
    [current-timezone (Boxof (U tz #f))]
    [posix->moment (-> Exact-Rational tz Moment)]
    [moment->datetime/local (-> Moment DateTime)]
    [UTC String]
    [moment (->* (Natural)
                 (Month Natural Natural Natural Natural Natural
                        (U tz #f)
                        (-> (U tzgap tzoverlap) DateTime (U String #f) (U #f Moment) Moment))
                 Moment)]
    [moment=? (-> Moment Moment Boolean)]
    [moment->iso8601 (-> Moment String)]
    [moment->iso8601/tzid (-> Moment String)]
)
(require/typed/check "datetime.rkt"
    [datetime->date (-> DateTime Date)]
    [datetime->time (-> DateTime Time)]
)

;; =============================================================================

(: now/moment (->* () ((U tz #f)) Moment))
(define (now/moment [tz (unbox current-timezone)])
  (unless tz (error "current-timezone is #f"))
  (posix->moment ((unbox current-clock)) tz))

(: now/moment/utc (-> Moment))
(define (now/moment/utc)
  (now/moment "Etc/UTC"))

(: now (->* () ((U tz #f)) DateTime))
(define (now [tz (unbox current-timezone)])
  (unless tz (error "now: current-timezone is #f"))
  (moment->datetime/local (now/moment tz)))

(: now/utc (-> DateTime))
(define (now/utc)
  (now "Etc/UTC"))

(: today (->* () ((U tz #f)) Date))
(define (today [tz (unbox current-timezone)])
  (unless tz (error "today: current-timezone is #f"))
  (datetime->date (now tz)))

(: today/utc (-> Date))
(define (today/utc)
  (today "Etc/UTC"))

(: current-time (->* () ((U tz #f)) Time))
(define (current-time [tz (unbox current-timezone)])
  (unless tz (error "current-time:  current-timezone is #f"))
  (datetime->time (now tz)))

(: current-time/utc (-> Time))
(define (current-time/utc)
  (current-time "Etc/UTC"))

(: current-posix-seconds (-> Natural))
(define (current-posix-seconds)
  (let ([r (/ (inexact->exact (current-inexact-milliseconds)) 1000)])
    (unless (index? r) (error "current-posix-seconds"))
    r))

(: current-clock (Boxof (-> Exact-Rational)))
(define current-clock (box current-posix-seconds))
