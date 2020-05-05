#lang typed/racket/base

(require scv-cr/opaque)

(require/typed/opaque "abs.rkt"
  [absz (-> Real Number)]
  [#:opaque Pict pict?])
