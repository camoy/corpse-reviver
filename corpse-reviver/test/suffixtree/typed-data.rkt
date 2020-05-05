#lang typed/racket/base

(provide Label Tree Node
  (struct-out label)
  (struct-out node)
  (struct-out suffix-tree)
  set-label-i!
  set-label-j!
  set-label-datum!
  set-node-children!
  set-node-up-label!
  set-node-parent!
  set-node-suffix-link!)

(require scv-cr/require-typed-check)

(require/typed/check "data.rkt"
  [#:struct label ([datum : (Vectorof (U Char Symbol))]
                   [i : Natural] [j : Natural])]
  [set-label-i! (-> Label Natural Void)]
  [set-label-j! (-> Label Natural Void)]
  [set-label-datum! (-> Label (Vectorof (U Char Symbol)) Void)]
  [#:struct node ([up-label : Label]
                  [parent : (U #f Node)]
                  [children : (Listof Node)]
                  [suffix-link : (U #f Node)])]
  [set-node-children! (-> Node (Listof Node) Void)]
  [set-node-up-label! (-> Node Label Void)]
  [set-node-parent! (-> Node Node Void)]
  [set-node-suffix-link! (-> Node Node Void)]
  [#:struct suffix-tree ([root : Node])])

(define-type Label label)
(define-type Tree suffix-tree)
(define-type Node node)

