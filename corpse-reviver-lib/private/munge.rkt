#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract
         "data.rkt")
(provide
 (contract-out
  [munge (-> symbol? syntax? libs/c syntax?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/contract
         racket/list
         syntax/parse
         syntax/strip-context
         threading
         "syntax.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; munge

;; Symbol Syntax Libs → Syntax
;; Given an identifier of a contract definition, munges that contract for use in
;; verification.
(define (munge ctc-sym stx libs)
  (define stx*
    (let go ([stx stx])
      (syntax-parse stx
        #:datum-literals (lambda
                          equal?
                          quote
                          ->*
                          simple-result->
                          any-wrap/c
                          pred-cnt
                          flat-named-contract
                          flat-contract-predicate
                          struct-predicate-procedure?/c
                          struct-predicate-procedure?
                          struct-type/c
                          letrec
                          c->
                          c->*
                          typed-racket-hash/c
                          mutable-vector/c
                          immutable-vector/c
                          mutable-vectorof/c
                          immutable-vectorof/c)
        ;; Convert lifted identifiers
        [lifted:id
         #:when (lifted->l #'lifted)
         (lifted->l #'lifted)]

        ;; Convert any-wrap/c to any/c (SCV)
        [any-wrap/c #'any/c]

        ;; Contract for predicate checking
        [pred-cnt #'(-> any/c boolean?)]

        ;; TR hash
        [typed-racket-hash/c #'hash/c]

        ;; TR vectors
        [(mutable-vector/c x ...) (go #'(vector/c x ...))]
        [(immutable-vector/c x ...) (go #'(vector/c x ...))]
        [(mutable-vectorof/c x) (go #'(vectorof x))]
        [(immutable-vectorof/c x) (go #'(vectorof x))]

        ;; Inline simple-result->, cannot require (SCV)
        [(simple-result-> ran arity)
         (go #`(-> #,@(for/list ([_ (syntax->datum #'arity)]) #'any/c) ran))]

        ;; Convert c-> and c->* to -> and ->* (SCV)
        [(c-> x ...) (go #'(-> x ...))]
        [(c->* x ...) (go #'(->* x ...))]

        ;; Convert ->* to -> if possible (SCV)
        [(->* (dom ...) () ran)
         (go #'(-> dom ... ran))]

        ;; Rest with no optional arguments (SCV)
        [(->* (man-dom ...) () #:rest rst rng)
         (go #'(->* (man-dom ...) #:rest rst rng))]

        ;; ->* with optional arguments to case->
        [(->* (man-dom ...) (optional-dom ...) rng)
         #:with (c ...) (let ([os (syntax->list #'(optional-dom ...))])
                          (for/list ([k (add1 (length os))])
                            #`(-> man-dom ... #,@(take os k) rng)))
         (go #'(case-> c ...))]

        ;; Replace contracts we cannot verify (SCV)
        [struct-predicate-procedure? #'(λ (x) #f)]
        [struct-predicate-procedure?/c #'(λ (x) #f)]
        [(struct-type/c _) #'struct-type?]

        ;; Unwrap some contract forms (SCV)
        [(flat-named-contract _ ctc)
         (go #'ctc)]

        [(flat-contract-predicate v)
         (go #'v)]

        ;; Replace literal voids with call to void function
        [(quote y)
         #:when (void? (syntax-e #'y))
         #'(void)]

        ;; Distribute munge-contract to all list elements
        [(x ...) (datum->syntax #f (map go (attribute x)))]

        ;; Catch-all
        [other #'other])))

  (syntax-property (adjust-scopes stx* libs)
                   'parent-identifier
                   ctc-sym))

;; Syntax Libs → Syntax
;; Strips context from identifiers that should be locally defined (references to
;; defined contracts) and attaches a scope to everything else. This scope will be
;; flipped later so the original scopes are preserved.
(define (adjust-scopes stx libs)
  (syntax-parse stx
    [(x ...) (datum->syntax #f (map (λ~> (adjust-scopes libs)) (attribute x)))]

    ;; Protect scopes on foreign syntax. We don't consider contract identifiers
    ;; foreign syntax since their meaning should come from SCV (i.e.
    ;; fake-contract).
    [x:id
     #:when (not (or (locally-defined-id? #'x) (expanded-or-contract-id? #'x)))
     (syntax-property (contract-sc #'x) 'protect-scope #t)]

    ;; Identifier came from require/typed. We must attach that import-specific
    ;; scope.
    [x:id
     #:when (hash-has-key? libs (syntax-e #'x))
     (syntax-property
      (replace-context (hash-ref libs (syntax-e #'x)) #'x) 'protect-scope #t)]

    ;; These scopes will be erased later.
    [x #'x]))

;; Identifier → Boolean
;; Returns if this is a locally defined contract and should have it's scopes
;; stripped. We can re-use the symbol->number function for this purpose.
(define (locally-defined-id? x)
  (symbol->number (syntax-e x)))

;; Identifier → Boolean
;; Returns whether identifier came from expansion or is a contract identifier.
(define (expanded-or-contract-id? e)
  (define mpi (and~> e identifier-binding third))
  (define root-mpi (and~> mpi module-path-index-root))
  (define name (and~> mpi module-path-index-resolve resolved-module-path-name))
  (or (equal? root-mpi 'racket/contract)
      (equal? name '|expanded module|)))

;; Module-Path-Index → Module-Path
;; Returns the root module path of an MPI.
(define (module-path-index-root mpi)
  (define-values (mp base) (module-path-index-split mpi))
  (if base (module-path-index-root base) mp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           "compile.rkt")

  (define CWD (path->complete-path "."))
  (define any/c-mod-stx
    (expand/dir CWD #'(module blah racket any/c)))
  (define any/c-stx
    (syntax-parse any/c-mod-stx
      [(m _ _ (mb _ (app _ (lam _ x) _))) #'x]))
  (define x-mod-stx
    (expand/dir CWD #'(module blah racket (define x 10) x)))
  (define x-stx
    (syntax-parse x-mod-stx
      [(m _ _ (mb _ _ (app _ (lam _ x) _))) #'x]))
  (define lam-stx
    (syntax-parse x-mod-stx
      [(m _ _ (mb _ _ (app _ (lam _ x) _))) #'lam]))

  (with-chk (['name "munge"])
    (define munge-e (λ~> (munge '_ _ (hash)) syntax->datum))
    (chk
     (munge-e #'lifted/1) 'l1
     (munge-e #'any-wrap/c) 'any/c
     (munge-e #'pred-cnt) '(-> any/c boolean?)
     (munge-e #'typed-racket-hash/c) 'hash/c
     (munge-e #'(mutable-vector/c any-wrap/c)) '(vector/c any/c)
     (munge-e #'(mutable-vector/c any/c)) '(vector/c any/c)
     (munge-e #'(immutable-vector/c any/c)) '(vector/c any/c)
     (munge-e #'(mutable-vectorof/c any/c)) '(vectorof any/c)
     (munge-e #'(immutable-vectorof/c any/c)) '(vectorof any/c)
     (munge-e #'(simple-result-> integer? 2)) '(-> any/c any/c integer?)
     (munge-e #'(c-> integer? integer?)) '(-> integer? integer?)
     (munge-e #'(c->* (integer?) () integer?)) '(-> integer? integer?)
     (munge-e #'(->* (integer?) () integer?)) '(-> integer? integer?)
     (munge-e #'(->* (integer?) () #:rest (listof integer?) any/c))
     '(->* (integer?) #:rest (listof integer?) any/c)
     (munge-e #'(->* (integer?) (string? any/c) real?))
     '(case-> (-> integer? real?)
              (-> integer? string? real?)
              (-> integer? string? any/c real?))
     (munge-e #'struct-predicate-procedure?) '(λ (x) #f)
     (munge-e #'struct-predicate-procedure?/c) '(λ (x) #f)
     (munge-e #'(struct-type/c _)) 'struct-type?
     (munge-e #'(flat-named-contract blah integer?)) 'integer?
     (munge-e #'(flat-contract-predicate integer?)) 'integer?
     (munge-e #`(quote #,(void))) '(void)))

  (with-chk (['name "adjust-scopes"])
    (define adjust-scopes* (λ~> (adjust-scopes (hash))))
    (chk
     #:t (syntax-property (adjust-scopes* #'blah) 'protect-scope)
     #:t (syntax-property (adjust-scopes* lam-stx) 'protect-scope)
     #:! #:t (syntax-property (adjust-scopes* x-stx) 'protect-scope)
     #:! #:t (syntax-property (adjust-scopes* any/c-stx) 'protect-scope)))

  (with-chk (['name "locally-defined-id?"])
    (chk
     #:t (locally-defined-id? #'g5)
     #:t (locally-defined-id? #'generated-contract5)
     #:t (locally-defined-id? #'l17)
     #:! #:t (locally-defined-id? #'blah)))

  (with-chk (['name "expanded-or-contract-id?"])
    (chk
     #:t (expanded-or-contract-id? any/c-stx)
     #:t (expanded-or-contract-id? x-stx)
     #:! #:t (expanded-or-contract-id? lam-stx)))

  (with-chk (['name "module-path-index-root"])
    (define mpi (and~> any/c-stx identifier-binding third))
    (chk
     (module-path-index-root mpi) 'racket/contract))
  )
