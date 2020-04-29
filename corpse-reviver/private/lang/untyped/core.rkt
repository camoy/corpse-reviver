#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (rename-out [-require require])
         (rename-out [require racket:require])
         (all-from-out racket/require)
         filter-safe
         register-unsafe-hash!
         (for-syntax (all-from-out racket/base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/function
                     racket/require-transform
                     syntax/modresolve
                     syntax/parse
                     syntax/strip-context)
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
(define-syntax (-require stx)
  (syntax-parse stx
    [(_ mods ...)
     (define reqs
       (for/list ([mod (in-list (attribute mods))])
         (define mod-string
           (path->string (resolve-module-path (syntax->datum mod))))
         (replace-context
          stx
          (if (hash-has-key? unsafe-hash mod-string)
              #`(racket:require
                 (subtract-in #,mod (filter-safe #,mod #,mod))
                 (filter-safe #,mod (submod #,mod provide/unsafe)))
              #`(racket:require #,mod)))))
     #`(begin #,@reqs)]))

;; Helpers require transformer for filtering only safe bindings.
(define-syntax filter-safe
  (make-require-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ base path)
        (define path-string
          (path->string (resolve-module-path (syntax->datum #'base))))
        (define unsafes (hash-ref unsafe-hash path-string))
        (define-values (imports import-srcs)
          (expand-import #'path))
        (values (filter (λ (import)
                          (not (member (import-src-sym import) unsafes)))
                        imports)
                import-srcs)]))))
