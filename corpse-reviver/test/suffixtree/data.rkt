#lang typed/racket/base

(provide
  (struct-out label)
  (struct-out suffix-tree)
  (struct-out node)
  make-label
  make-suffix-tree
  make-node)


(struct label ([datum : (Vectorof (U Char Symbol))] [i : Natural] [j : Natural]) #:mutable)

;; A suffix tree consists of a root node.
(struct suffix-tree ([root : node]))

;; up-label: label
;; parent: (union #f node)
;; children: (listof node)
;; suffix-link: (union #f node)
(struct node ([up-label : label] [parent : (U #f node)] [children : (Listof node)] [suffix-link : (U #f node)]) #:mutable)

(: make-label (-> (Vectorof (U Char Symbol)) Natural Natural label))
(define make-label label)

(: make-suffix-tree (-> node suffix-tree))
(define make-suffix-tree suffix-tree)

(: make-node (-> label (U #f node) (Listof node) (U #f node) node))
(define make-node node)
