#lang typed/racket/base

(require
  corpse-reviver/require-typed-check
  "core-adapter.rkt")

(require/typed/check "gregor-structs.rkt"
  [#:struct Date ([ymd : YMD]
                  [jdn : Integer])]
  [#:struct Time ([hmsn : HMSN]
                  [ns : Natural])]
  [#:struct DateTime ([date : Date]
                      [time : Time]
                      [jd : Exact-Rational])]
  [#:struct Moment ([datetime/local : DateTime]
                    [utc-offset : Integer]
                    [zone : (U String #f)])]
)

(provide (struct-out Date)
         (struct-out Time)
         (struct-out DateTime)
         (struct-out Moment))
