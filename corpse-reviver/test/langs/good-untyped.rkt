#lang s-exp corpse-reviver/private/lang/opt/untyped/base

(require (for-syntax racket/base))

(define-syntax (do-register-unsafe-hash _)
  (define server-path
    (path->string (path->complete-path "ty-server.rkt")))
  (define h (hash server-path '(adder)))
  #`(register-unsafe-hash! #,h))
(do-register-unsafe-hash)

(require "ty-server.rkt")
(adder "hi")
