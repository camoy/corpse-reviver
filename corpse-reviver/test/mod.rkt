#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide cleanup-bytecode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax
                     syntax/transformer
                     "path.rkt"
                     "util.rkt")
         mischief/memoize
         syntax/parse/define
         "path.rkt"
         "../private/compile.rkt"
         "../private/elaborate.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-mod/memo (memoize-procedure make-mod))

(define-simple-macro (provide-test-mods)
  #:with [[?x . ?y] ...] (hash->list TEST-PATHS)
  #:with [?x-mod ...] (syntax-suffix "~a-mod" #'(?x ...))
  (begin
    (provide ?x-mod ...)
    (define-syntax ?x-mod
      (make-variable-like-transformer
       #'(make-mod/memo '?y))) ...))

(define (cleanup-bytecode)
  (for-each delete-bytecode (hash-values TEST-PATHS)))

(provide-test-mods)
