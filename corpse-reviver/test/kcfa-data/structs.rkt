#lang typed/racket/base

(provide
  (struct-out Stx)
  (struct-out exp))

(struct Stx
 ([label : Symbol]))

(struct exp Stx ())
