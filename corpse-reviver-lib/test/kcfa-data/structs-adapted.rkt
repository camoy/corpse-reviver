#lang typed/racket/base

(require corpse-reviver/require-typed-check)

(require/typed/check "structs.rkt"
  [#:struct Stx ([label : Label])]
  [#:struct (exp Stx) ()])

(provide
  (struct-out Stx)
  (struct-out exp)
  Exp)

(define-type Exp exp)
