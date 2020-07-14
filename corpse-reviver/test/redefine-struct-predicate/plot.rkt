#lang typed/racket/base

(provide
  plot-font-size
  (struct-out pict))

(: plot-font-size (-> Number (Listof pict)))
(define (plot-font-size n)
  null)

(struct pict ())
