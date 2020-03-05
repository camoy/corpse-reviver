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

(define-simple-macro (define-test-files (~seq ?x:id ?y:string) ...)
  (begin
    (begin
      (provide ?x)
      (define ?x (simple-form-path (build-path TEST-DIR ?y))))
    ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test-files
  also-typed "also-typed.rkt"
  a "a.rkt"
  b "b.rkt"
  c "c.rkt"
  hello-world "hello-world.rkt"
  typed "typed.rkt"
  untyped "untyped.rkt"

  ty-ty-client "ty-ty/client.rkt"
  ty-ty-server "ty-ty/server.rkt"
  ty-ut-client "ty-ut/client.rkt"
  ty-ut-server "ty-ut/server.rkt"
  ut-ty-client "ut-ty/client.rkt"
  ut-ty-server "ut-ty/server.rkt"
  ut-ut-client "ut-ut/client.rkt"
  ut-ut-server "ut-ut/server.rkt")
