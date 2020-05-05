#lang typed/racket/base

(require
  scv-cr/require-typed-check
  "structs-adapted.rkt"
)

(require/typed/check "benv.rkt"
  [#:struct Closure
    ([lam : Lam]
     [benv : BEnv])]
  [#:struct Binding
    ([var : Var]
     [time : Time])]
  (empty-benv BEnv)
  (benv-lookup (-> BEnv Var Addr))
  (benv-extend (-> BEnv Var Addr BEnv))
  (benv-extend* (-> BEnv (Listof Var) (Listof Addr) BEnv))
)
(provide
  (struct-out Closure)
  (struct-out Binding)
  empty-benv
  benv-lookup
  benv-extend
  benv-extend*
  BEnv
  Addr
  Time
)

;; =============================================================================

(define-type BEnv (HashTable Var Addr))
(define-type Addr Binding)
(define-type Time (Listof Label))
