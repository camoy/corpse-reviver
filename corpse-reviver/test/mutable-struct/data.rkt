#lang racket/base

(provide (struct-out node))
(struct node (suffix-link) #:mutable)
