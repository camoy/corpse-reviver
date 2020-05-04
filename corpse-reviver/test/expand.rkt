#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax
                     syntax/transformer
                     "path.rkt"
                     "util.rkt")
         mischief/memoize
         syntax/parse/define
         "../private/syntax.rkt"
         "../private/compile.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define expand/dir/memo (memoize-procedure expand/dir))

(define-simple-macro (provide-test-expansions)
  #:with [[?x . ?y] ...] (hash->list TEST-PATHS)
  #:with [?x-stx ...] (syntax-suffix "~a-stx" #'(?x ...))
  #:with [?x-expand ...] (syntax-suffix "~a-expand" #'(?x ...))
  (begin
    (provide ?x-stx ... ?x-expand ...)
    (define ?x-stx (syntax-fetch '?y)) ...
    (define-syntax ?x-expand
      (make-variable-like-transformer
       #'(expand/dir/memo '?y ?x-stx))) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide-test-expansions)
