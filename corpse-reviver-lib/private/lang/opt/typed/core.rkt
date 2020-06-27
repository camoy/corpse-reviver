#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (rename-out [-require/typed require/typed])
         (rename-out [-require/typed/provide require/typed/provide])
         (rename-out [-provide provide])
         register-unsafe-hash!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax mischief/for
                     racket/syntax
                     racket/base
                     racket/sequence
                     racket/list
                     syntax/strip-context
                     syntax/modresolve
                     syntax/parse
                     "../../../syntax.rkt")
         (only-in typed/racket/base require/typed require/typed/provide)
         typed/racket/unsafe
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

(begin-for-syntax
  ;; Conservatively, if we don't have any information on an identifier, we assume
  ;; it's unsafe.
  (define (unsafe-id? m id)
    (define m-path (path->string (resolve-module-path (syntax->datum m))))
    (define unsafes (hash-ref unsafe-hash m-path (λ () #f)))
    (or (not unsafes)
        (member (syntax->datum id) unsafes)))

  (define (clauses m clauses ids unsafe?)
    (for/filter ([clause (in-syntax clauses)]
                 [id (in-syntax ids)])
      (cond
        [unsafe? (and (unsafe-id? m id) clause)]
        [else (and (not (unsafe-id? m id)) clause)])))

  (define (void-if-empty x ys)
    (if (empty? (syntax->list ys))
        #'(void)
        x))

  ;; Rewrite require to import safe bindings from the uncontracted submodule.
  (define ((make-require/typed provide?) stx)
    (syntax-parse stx
      [(_ m c:clause ...)
       #:with ?provides ; strip-context to use -provide
       (if provide? (replace-context stx #'(provide c.out ... ...)) #'(void))
       (if (syntax-property #'m 'opaque)
           #'(begin ?provides
                    (require/typed m c ...))
           (with-syntax* ([(?c-unsafe ...)
                           (clauses #'m #'(c ...) #'(c.x ...) #t)]
                          [(?c-safe ...)
                           (clauses #'m #'(c ...) #'(c.x ...) #f)]
                          [?require/typed
                           (void-if-empty #'(require/typed m ?c-unsafe ...)
                                          #'(?c-unsafe ...))]
                          [?unsafe-require/typed
                           (void-if-empty #'(unsafe-require/typed m ?c-safe ...)
                                          #'(?c-safe ...))])
             #'(begin ?provides
                      ?require/typed
                      ?unsafe-require/typed)))]))
  )

(define-syntax -require/typed (make-require/typed #f))
(define-syntax -require/typed/provide (make-require/typed #t))

(define-syntax (-provide stx)
  (syntax-case stx ()
    [(_ spec ...)
     #'(begin
         (provide spec ...)
         (module+ unsafe (unsafe-provide spec ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk)

  (chk
   #:x (dynamic-require "../../../test/langs/bad-typed.rkt" #f)
   "add1: contract violation"

   #:x (dynamic-require "../../../test/langs/good-typed.rkt" #f)
   "adder: broke its own contract"
   ))
