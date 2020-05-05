#lang racket/base

(require (prefix-in f: racket/format))
(provide ~r ~r* ~r**)

(define (~r n min-width pad-string)
  (f:~r n
      #:min-width min-width
      #:pad-string pad-string))

(define (~r* n precision)
  (f:~r n
      #:precision precision))

(define (~r** n min-width pad-string)
  (f:~r n
      #:min-width min-width
      #:pad-string pad-string
      #:sign #f))
