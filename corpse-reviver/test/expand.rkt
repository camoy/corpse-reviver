#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax
                     "path.rkt")
         syntax/parse/define
         racket/path
         racket/runtime-path
         "../private/syntax.rkt"
         "../private/compile.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-runtime-path TEST-DIR ".")

(begin-for-syntax
  (define (syntax-suffix fmt ids)
    (for/list ([x (in-syntax ids)])
      (format-id #f fmt x))))

(define-simple-macro (provide-test-expansions)
  #:with [[?x . ?y] ...] (hash->list TEST-PATHS)
  #:with [?x-stx ...] (syntax-suffix "~a-stx" #'(?x ...))
  #:with [?x-expand ...] (syntax-suffix "~a-expand" #'(?x ...))
  (begin
    (provide ?x ...)
    (define ?x (simple-form-path (build-path TEST-DIR '?y))) ...

    (provide ?x-stx ...)
    (define ?x-stx (syntax-fetch ?x)) ...

    (provide ?x-expand ...)
    (define ?x-expand (expand/dir ?x ?x-stx)) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide-test-expansions)
