#lang typed/racket/base

(require/typed/provide "plot.rkt"
  [#:struct pict ()]
  [plot-font-size (-> Real (Listof pict))])
