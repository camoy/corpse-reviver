#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (rename-out [require racket:require])
         (rename-out [-require require])
         (rename-out [-provide provide])
         (all-from-out racket/require)
         register-unsafe-hash!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/function
                     racket/require-transform
                     racket/set
                     racket/syntax
                     racket/pretty
                     syntax/modresolve
                     syntax/parse
                     syntax/strip-context
                     threading
                     "../../../util.rkt")
         racket/require)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros

(begin-for-syntax
  (define unsafe-hash #f)

  ;; [Syntax Module-Path] → String
  ;; Convert a module path (as syntax) to it's resolved absolute path (as a
  ;; string).
  (define (mod-path-stx->string mod-path-stx)
    (~> mod-path-stx
        syntax-e
        resolve-module-path
        path->string))

  ;; Import → Boolean
  ;; Returns if the import is safe according to the unsafe hash.
  (define (safe-import? import)
    (define src (mod-path-stx->string (import-src-mod-path import)))
    (define src-id (import-src-sym import))
    (and unsafe-hash
         (hash-has-key? unsafe-hash src)
         (not (member src-id (hash-ref unsafe-hash src)))))

  ;; Syntax → Syntax
  ;; Optimize a require spec by selectively importing safe bindings from the
  ;; unsafe submodule.
  (define (optimize-spec spec)
    (define-values (imports _) (expand-import spec))
    (define safe-imports (filter safe-import? imports))
    (define exclude (map import-local-id safe-imports))
    (define unsafe-mods
      (set->list
       (for/set ([safe-import (in-list safe-imports)])
         (let ([mod (syntax->datum (import-src-mod-path safe-import))])
           `(submod ,mod unsafe)))))
    (define renames
      (for/list ([safe-import (in-list safe-imports)])
        (list (import-src-sym safe-import) (import-local-id safe-import))))
    #`(combine-in
       (except-in #,spec #,@exclude)
       (only-in (combine-in #,@unsafe-mods) #,@renames))))

;; Registers the unsafe hash for use in the require form.
(define-syntax (register-unsafe-hash! stx)
  (syntax-parse stx
    [(_ h)
     (set! unsafe-hash (syntax->datum #'h))
     #'(void)]))

;; Rewrite require to import safe bindings from the uncontracted submodule.
(define-syntax (-require stx)
  (syntax-parse stx
    [(_ spec ...)
     (replace-context
      stx
      #`(racket:require #,@(map optimize-spec (syntax->list #'(spec ...)))))]))

;; Provide, but also with an unsafe submodule.
(define-syntax (-provide stx)
  (syntax-case stx ()
    [(_ spec ...)
     #'(begin
         (provide spec) ...
         (module+ unsafe (provide spec)) ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk)

  (chk
   #:x (dynamic-require "../../../test/langs/bad-untyped.rkt" #f)
   "add1: contract violation"

   #:x (dynamic-require "../../../test/langs/good-untyped.rkt" #f)
   "adder: contract violation"

   #:x (dynamic-require "../../../test/langs/bad-spec-untyped.rkt" #f)
   "add1: contract violation"

   #:x (dynamic-require "../../../test/langs/good-spec-untyped.rkt" #f)
   "adder: contract violation"
   ))
