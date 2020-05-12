#lang typed/racket/base

(require
  corpse-reviver/require-typed-check
  "structs-adapted.rkt"
  "benv-adapted.rkt"
  "time-adapted.rkt"
)

(require/typed/check "denotable.rkt"
  [#:struct State
    ([call : Exp]
     [benv : BEnv]
     [store : Store]
     [time : Time])]
   [d-bot Denotable]
   [d-join (-> Denotable Denotable Denotable)]
   [empty-store Store]
   [store-lookup (-> Store Addr Denotable)]
   [store-update (-> Store Addr Denotable Store)]
   [store-update* (-> Store (Listof Addr) (Listof Denotable) Store)]
   [store-join (-> Store Store Store)]
)

;; ---

(provide
  Denotable
  Store
  (struct-out State)
  d-bot
  d-join
  empty-store
  store-lookup
  store-update
  store-update*
  store-join
)

;; =============================================================================

(define-type Denotable (Setof Value))
(define-type Store (HashTable Addr Denotable))

