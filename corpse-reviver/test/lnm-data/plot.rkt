#lang typed/racket/base

(provide
  plot-font-size
  pict?)

(: plot-font-size (-> Number Void))
(define (plot-font-size n)
  (void))

(: pict? (-> Any Boolean))
(define (pict? x) #f)
