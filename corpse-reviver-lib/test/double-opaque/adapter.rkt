#lang typed/racket/base

(require/typed/provide "abs.rkt"
  [absz (-> Real Number)]
  [#:opaque Pict pict?])
