#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (rename-out [require racket:require])
         (rename-out [-require require])
         (all-from-out racket/require)
         register-unsafe-hash!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/function
                     racket/require-transform
                     racket/syntax
                     racket/pretty
                     syntax/modresolve
                     syntax/parse
                     syntax/strip-context
                     threading
                     "../../util.rkt")
         racket/require)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros

(begin-for-syntax
  (define unsafe-hash #f))

;; Registers the unsafe hash for use in the require form.
(define-syntax (register-unsafe-hash! stx)
  (syntax-parse stx
    [(_ h)
     (set! unsafe-hash (syntax->datum #'h))
     #'(void)]))

;; Rewrite require to import safe bindings from the uncontracted submodule.
;; (We steal the trick of faking a typed module from an untyped from the
;; live-free-or-die package).
(define-syntax (-require stx)
  (syntax-parse stx
    [(_ mods ...)
     (define reqs
       (for/list ([mod (in-list (attribute mods))])
         (define mod-string
           (and~>> mod
                   syntax->datum
                   (satisfies module-path?)
                   resolve-module-path
                   path->string))
         (cond
           [(and unsafe-hash (hash-has-key? unsafe-hash mod-string))
            (define unsafes (hash-ref unsafe-hash mod-string))
            (define unsafe-mod (format-symbol "~a/unsafe" (syntax->datum mod)))
            (replace-context
             stx
             #`(begin
                 (racket:require (only-in #,mod #,@unsafes))
                 (racket:require (except-in (submod #,mod unsafe) #,@unsafes))))]
           [else #`(require #,mod)])))
     #`(begin #,@reqs)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk)

  (chk
   #:x (dynamic-require "../../../test/langs/bad-untyped.rkt" #f)
   "add1: contract violation"

   #:x (dynamic-require "../../../test/langs/good-untyped.rkt" #f)
   "adder: contract violation"
   ))
