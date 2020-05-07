#lang racket

(require
  (for-syntax
    typed/untyped-utils
    syntax/parse)
  (prefix-in typed: (only-in typed/racket require))
)

;; =============================================================================

(define-for-syntax typed-lib?
  (let ([cache (make-hash)])
    (λ (lib-stx)
      (let* ([lib (syntax->datum lib-stx)]
             [dyn-path (and (not (relative-submod? lib))
                            (append
                              (if (submod? lib)
                                (resolve-submod #f lib)
                                (list 'submod lib))
                              '(#%type-decl)))])
        (hash-ref! cache lib
          (λ () ;; Typed Racket always installs a `#%type-decl` submodule
            (and dyn-path
                 (module-declared? dyn-path #true)
                 #true)))))))

;; : Require-Spec -> Boolean
(define-for-syntax (submod? x)
  (and (list? x) (not (null? x)) (eq? 'submod (car x))))

;; : Require-Spec -> Boolean
(define-for-syntax (relative-submod? x)
  (and (submod? x)
       (not (null? (cdr x)))
       (or (string=? "." (cadr x))
           (string=? ".." (cadr x)))))

;; : Submod-Path -> Module-Path
(define-for-syntax (resolve-submod src l)
  (case (cadr l)
   [(".." ".")
    ;; Circular dependency issue ... cannot compile the module without
    ;;  compiling the module's submodules.
    (raise-argument-error 'require/typed/check "Non-relative submodule" l)]
   [else
    l]))

(define-syntax (require/typed/check stx)
  (syntax-parse stx
    #:literals (prefix-in)
    [(_ (prefix-in p m:str) rt-clause ...)
     (cond
       [(not (syntax-local-typed-context?))
        #`(#,(datum->syntax stx 'require) (prefix-in p m))]
       [(and (syntax-local-typed-context?)
             (typed-lib? #'m))
        #'(typed:require (prefix-in p m))]
       [else
        #`(#,(datum->syntax stx 'require/typed) m rt-clause ...)])]
    [(_ m:str rt-clause ...)
     (cond
       [(not (syntax-local-typed-context?))
        #`(#,(datum->syntax stx 'require) m)]
       [(and (syntax-local-typed-context?)
             (typed-lib? #'m))
        #'(typed:require m)]
       [else
        #`(#,(datum->syntax stx 'require/typed) m rt-clause ...)])]))

(provide
  require/typed/check)
