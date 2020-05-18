#lang typed/racket/base

(require corpse-reviver/opaque)

(require/typed/provide/opaque "abs.rkt"
  [absz (-> Real Number)]
  [#:opaque Pict pict?])
