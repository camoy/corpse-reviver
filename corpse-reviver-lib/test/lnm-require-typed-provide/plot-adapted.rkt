#lang typed/racket/base

(require corpse-reviver/opaque)

(require/typed/provide/opaque "_plot.rkt"
  [line-width (-> Nonnegative-Real)])
