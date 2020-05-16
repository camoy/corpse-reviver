#lang racket/base

(require (prefix-in f: racket/format))
(provide ~r)
(define (~r n base min-width pad-string)
  (f:~r n
      #:base base
      #:min-width min-width
      #:pad-string pad-string))
