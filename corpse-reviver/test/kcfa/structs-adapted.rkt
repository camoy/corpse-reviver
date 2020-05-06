#lang typed/racket/base

(require
  corpse-reviver/require-typed-check
)

(require/typed/check "structs.rkt"
  [#:struct Stx ([label : Label])]
  [#:struct (exp Stx) ()]
  [#:struct (Ref exp) ([var : Var])]
  [#:struct (Lam exp) ([formals : (Listof Var)] [call : Exp])]
  [#:struct (Call Stx) ([fun : Exp] [args : (Listof Exp)])]
)

(provide
  (struct-out Stx)
  (struct-out exp)
  (struct-out Ref)
  (struct-out Lam)
  (struct-out Call)
  Exp
  Label
  Var
)

;; =============================================================================

(define-type Exp (U exp Ref Lam Call))
(define-type Label Symbol)
(define-type Var Symbol)

