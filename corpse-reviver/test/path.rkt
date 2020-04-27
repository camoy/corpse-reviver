#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide TEST-DIR)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (for-syntax racket/base)
         syntax/parse/define
         racket/path
         racket/runtime-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-runtime-path TEST-DIR ".")

(define-simple-macro (provide-test-paths (~seq ?x:id ?y:string) ...)
  (begin
    (provide TEST-PATHS)
    (define TEST-PATHS (hash (~@ '?x ?y) ...))

    (provide ?x ...)
    (define ?x (simple-form-path (build-path TEST-DIR ?y))) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide-test-paths
  also-typed "etc/also-typed.rkt"
  a "etc/a.rkt"
  b "etc/b.rkt"
  c "etc/c.rkt"
  hello-world "etc/hello-world.rkt"
  typed "etc/typed.rkt"
  untyped "etc/untyped.rkt"

  predicate-main "predicate/main.rkt"
  predicate-server "predicate/server.rkt"

  ty-ty-client "ty-ty/client.rkt"
  ty-ty-server "ty-ty/server.rkt"
  ty-ut-client "ty-ut/client.rkt"
  ty-ut-server "ty-ut/server.rkt"
  ut-ty-client "ut-ty/client.rkt"
  ut-ty-server "ut-ty/server.rkt"
  ut-ut-client "ut-ut/client.rkt"
  ut-ut-server "ut-ut/server.rkt"

  sieve-main "sieve/main.rkt"
  streams "sieve/streams.rkt")
