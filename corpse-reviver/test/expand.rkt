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

(define-simple-macro (provide-test-expansions)
  #:with [[?x . ?y] ...] (hash->list TEST-PATHS)
  #:with [?z ...]
  (for/list ([x (in-syntax #'(?x ...))])
    (format-id #f "~a-expand" x))
  (begin
    (provide ?x ...)
    (define ?x (simple-form-path (build-path TEST-DIR '?y))) ...

    (provide ?z ...)
    (define ?z (expand/dir ?x (syntax-fetch ?x))) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide-test-expansions)
