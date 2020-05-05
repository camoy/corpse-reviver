#lang racket

(require
  (for-syntax
    typed/untyped-utils
    syntax/parse)
  (only-in typed/racket require/typed)
  (prefix-in typed: (only-in typed/racket require))
)

;; =============================================================================

(define-syntax (require/typed/check stx)
  (syntax-parse stx 
    #:literals (prefix-in)
    [(_ (prefix-in p m:str) rt-clause ...)
     (cond 
       [(not (syntax-local-typed-context?))
        #'(require (prefix-in p m))]
       [(and (syntax-local-typed-context?)
             (module->language-info (syntax->datum #'m) #t))
        #'(typed:require (prefix-in p m))]
       [else 
        #'(require/typed m rt-clause ...)])]
    [(_ m:str rt-clause ...)
     (cond 
       [(not (syntax-local-typed-context?))
        #'(require m)]
       [(and (syntax-local-typed-context?)
             (module->language-info (syntax->datum #'m) #t))
        #'(typed:require m)]
       [else 
        #'(require/typed m rt-clause ...)])]))

(provide
  require/typed/check)

