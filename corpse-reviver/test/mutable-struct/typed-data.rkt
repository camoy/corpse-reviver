#lang typed/racket/base

(provide
  (struct-out node)
  set-node-suffix-link!)

(require/typed "data.rkt"
  [#:struct node ([suffix-link : (U #f node)])]
  [set-node-suffix-link! (-> node node Void)])
