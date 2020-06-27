#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide TEST-DIR)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (for-syntax racket/base)
         syntax/parse/define
         racket/path
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-macro (provide-test-paths (~seq ?x:id ?y:string) ...)
  (begin
    (provide TEST-PATHS ?x ...)
    (define ?x (path->string (simple-form-path (build-path TEST-DIR ?y)))) ...
    (define TEST-PATHS (hash (~@ '?x ?x) ...))))

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

  sieve-main "sieve-fast/main.rkt"
  streams "sieve-fast/streams.rkt"

  sieve-slow-main "sieve-slow/main.rkt"
  sieve-slow-streams "sieve-slow/streams.rkt")
