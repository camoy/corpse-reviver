#lang racket/base

(provide (all-defined-out))

(struct tzoffset
  (utc-seconds
   dst?
   abbreviation)
  #:transparent)

(struct tzgap
  (starts-at
   offset-before
   offset-after)
  #:transparent)

(struct tzoverlap
  (offset-before
   offset-after)
  #:transparent)

(struct interval
  (from
   to
   offset)
  #:transparent)

(struct zone
  (id
   intervals
   offsets)
  #:transparent)

(struct tabzone
  (id
   country-codes
   coordinates
   comments)
  #:transparent)

(struct coordinates
  (latitude
   longitude)
  #:transparent)

(struct exn:fail:tzinfo exn:fail () #:transparent)
(struct exn:fail:tzinfo:zone-not-found exn:fail:tzinfo () #:transparent)
