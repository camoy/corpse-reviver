#lang racket/base

(provide
  (struct-out label)
  (struct-out suffix-tree)
  (struct-out node))


(struct label (datum i j) #:mutable)

;; A suffix tree consists of a root node.
(struct suffix-tree (root))

;; up-label: label
;; parent: (union #f node)
;; children: (listof node)
;; suffix-link: (union #f node)
(struct node (up-label parent children suffix-link) #:mutable)
