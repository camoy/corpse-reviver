#lang s-exp corpse-reviver/private/lang/typed/base

(require (for-syntax racket/base))

(define-syntax (do-register-unsafe-hash _)
  (define server-path
    (path->string (path->complete-path "ut-server.rkt")))
  (define h (hash server-path '(adder)))
  #`(register-unsafe-hash! #,h))
(do-register-unsafe-hash)

(require/typed "ut-server.rkt"
  [adder (-> Number Number)])

(add1 (adder 1))
