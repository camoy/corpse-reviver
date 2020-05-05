#lang typed/racket/base

(provide lnm-plot)

(require "plot-adapted.rkt")

(: lnm-plot (-> (Listof Pict)))
(define (lnm-plot)
  (plot-font-size 16)
  null)
