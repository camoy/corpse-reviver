#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide TEST-DIR)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (for-syntax racket/base
                     racket/syntax
                     threading)
         syntax/parse/define
         racket/path
         racket/runtime-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-runtime-path CWD ".")
(define TEST-DIR (build-path CWD ".." "test"))

(define-simple-macro (define-test-files ?x:id ...)
  #:with [?y ...]
  (map (λ~> (format-symbol "~a.rkt" _)
            symbol->string
            (datum->syntax this-syntax _))
       (attribute ?x))
  (begin
    (begin
      (provide ?x)
      (define ?x (simple-form-path (build-path TEST-DIR ?y))))
    ...))

(define-test-files
  also-typed
  a b c
  hello-world
  typed
  untyped)
