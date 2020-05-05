#lang typed/racket/base

;; Adapter module for the tzinfo subproject

(require scv-cr/opaque)

(require/typed/provide/opaque "../base/tzinfo/main.rkt"
  [system-tzid (-> (U tz #f))]
  [#:struct tzgap ([starts-at : Natural]
                   [offset-before : tzoffset]
                   [offset-after : tzoffset])]
  [#:struct tzoffset ([utc-seconds : Integer]
                      [dst? : Boolean]
                      [abbreviation : String])]
  [#:struct tzoverlap ([offset-before : tzoffset]
                       [offset-after : tzoffset])]
  [local-seconds->tzoffset (-> String Integer (U tzoffset tzgap tzoverlap))]
  [utc-seconds->tzoffset (-> String Exact-Rational tzoffset)])

(provide tz)
(define-type tz (U String Integer))
