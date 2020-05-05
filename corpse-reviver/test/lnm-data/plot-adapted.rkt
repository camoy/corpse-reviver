#lang typed/racket/base

(require/typed/provide "plot.rkt"
  [#:opaque Pict pict?]
  [plot-font-size   (-> Real Void)])
