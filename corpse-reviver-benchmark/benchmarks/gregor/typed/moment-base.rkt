#lang typed/racket/base

;; Support for moment.rkt
;; (Works together with offset-resolvers.rkt)

(provide
 moment->iso8601
 moment->iso8601/tzid
 make-moment
)

;; -----------------------------------------------------------------------------

(require
  corpse-reviver/require-typed-check
  racket/match
  "gregor-adapter.rkt"
  corpse-reviver/opaque
)
(require/typed/check "datetime.rkt"
    [datetime->iso8601 (-> DateTime String)]
    )

(require/typed/opaque "fake-format.rkt"
  [~r** (-> Exact-Rational
            Nonnegative-Integer
            String
            String)])

;; =============================================================================

(: moment->iso8601/tzid (-> Moment String))
(define (moment->iso8601/tzid m)
  (: iso String)
  (define iso (moment->iso8601 m))
  (match m
    [(Moment _ _ z) #:when z (format "~a[~a]" iso z)]
    [_ iso]))

(: moment->iso8601 (-> Moment String))
(define (moment->iso8601 m)
  (match m
    [(Moment d 0 _)
     (string-append (datetime->iso8601 d) "Z")]
    [(Moment d o _)
     (define sign (if (< o 0) "-" "+"))
     (define sec  (abs o))
     (define hrs  (quotient sec 3600))
     (define min  (quotient (- sec (* hrs 3600)) 60))
     (format "~a~a~a:~a"
             (datetime->iso8601 d)
             sign
             (~r** hrs 2 "0")
             (~r** min 2 "0"))]))

(: make-moment (-> DateTime Integer (U String #f) Moment))
(define (make-moment dt off z)
  (Moment dt off (and z (string->immutable-string z))))
