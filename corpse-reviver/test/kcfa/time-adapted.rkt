#lang typed/racket/base

(require
  corpse-reviver/require-typed-check
  "structs-adapted.rkt"
  "benv-adapted.rkt"
)

(require/typed "time.rkt"
  [time-zero Time]
  [k (Parameterof Natural)]
  [tick (-> Stx Time Time)]
  [alloc (-> Time (-> Var Addr))]
)

;; ---

(provide
  time-zero
  k
  tick
  alloc
  Value
)

;; =============================================================================

;; -- time.rkt
(define-type Value Closure)

