#lang typed/racket/base

(provide lnm-plot)

(require "plot-adapted.rkt")

(: lnm-plot (-> (Listof pict)))
(define (lnm-plot)
  (plot-font-size 16))
