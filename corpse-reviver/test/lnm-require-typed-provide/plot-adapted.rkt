#lang typed/racket/base

(require corpse-reviver/opaque)

(require/typed/provide/opaque "fake-plot.rkt"
  [line-width (-> Nonnegative-Real)])
