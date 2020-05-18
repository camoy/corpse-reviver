#lang typed/racket/base

(provide
  (struct-out label)
  (struct-out suffix-tree)
  (struct-out node))


(struct: label ([datum : (Vectorof (U Char Symbol))] [i : Natural] [j : Natural]) #:mutable)

;; A suffix tree consists of a root node.
(struct: suffix-tree ([root : node]))

;; up-label: label
;; parent: (union #f node)
;; children: (listof node)
;; suffix-link: (union #f node)
(struct: node ([up-label : label] [parent : (U #f node)] [children : (Listof node)] [suffix-link : (U #f node)]) #:mutable)
