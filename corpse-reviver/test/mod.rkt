#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax
                     "path.rkt"
                     "util.rkt")
         syntax/parse/define
         "../private/elaborate.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-macro (provide-test-mods)
  #:with [[?x . ?y] ...] (hash->list TEST-PATHS)
  #:with [?x-mod ...] (syntax-suffix "~a-mod" #'(?x ...))
  (begin
    (provide ?x-mod ...)
    (define ?x-mod (make-mod '?y)) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide-test-mods)
