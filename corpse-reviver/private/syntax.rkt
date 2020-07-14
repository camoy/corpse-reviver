#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [normalize-srcloc (-> syntax? path-string? syntax?)]

  [syntax-property-values (-> syntax? any/c list?)]
  [scv-ignore (-> syntax? syntax?)]

  [syntax-fetch (-> path-string? syntax?)]
  [syntax-deps (-> (or/c syntax? pair?) (listof module-path?))]
  [typed? (-> path-string? boolean?)]

  [protect (-> syntax? syntax?)]
  [protected? (-> syntax? boolean?)]
  [syntax-parent (case-> (-> syntax? symbol? syntax?)
                         (-> syntax? (or/c #f symbol?)))]
  [strip-context* (-> syntax? syntax?)]
  [lifted->l (-> syntax? (or/c syntax? #f))]
  [contains-id? (-> syntax? identifier? boolean?)]
  [chase-codomain (-> definitions/c syntax? syntax?)])

 syntax-properties
 clause)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         lang-file/read-lang-file
         racket/function
         racket/list
         racket/match
         racket/pretty
         racket/set
         racket/string
         racket/syntax
         syntax/modcollapse
         syntax/modread
         syntax/parse
         threading
         "data.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; source locations

;; Syntax Path-String → Syntax
;; Normalize source location information in the syntax, preserving lexical
;; information.
(define (normalize-srcloc stx target)
  (replace-srcloc stx (normalize-srcloc* stx target)))

;; Syntax Syntax → Syntax
;; Replace a syntax's source locations with another's source locations. The two
;; syntaxes should have the same structure.
(define/contract (replace-srcloc stx srcloc-stx)
  (->i ([stx0 syntax?] [stx1 syntax?])
       #:pre (stx0 stx1) (syntax-datum-equal? stx0 stx1)
       [_ syntax?])
  (let go ([e stx]
           [ref srcloc-stx])
    (cond
      [(and (syntax? e) (syntax? ref))
       (define e* (go (syntax-e e) (syntax-e ref)))
       (datum->syntax e e* ref e)]
      [(and (syntax? e) (pair? ref))
       (define e* (go (syntax-e e) ref))
       (datum->syntax e e* #f e)]
      [(and (pair? e) (pair? ref))
       (cons (go (car e) (car ref))
             (go (cdr e) (cdr ref)))]
      [else e])))

;; Syntax Path-String → Syntax
;; Normalize syntax, but with a lose of lexical information.
(define (normalize-srcloc* stx target)
  (define s (pretty-format (syntax->datum stx)))
  (define s* (open-input-string (substring s 1)))
  (port-count-lines! s*)
  (read-syntax target s*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax properties

;; Syntax Symbol → [Set Any]
;; Same as syntax-property-values, but returns as a set.
(define (syntax-property-values* stx key)
  (let go ([stx stx])
    (let* ([e (syntax-e stx)]
           [val (syntax-property stx key)]
           [vals (flatten-cons val)])
      (if (proper-list? e)
          (apply set-union vals (map go e))
          vals))))

;; Syntax Symbol → [Listof Any]
;; Retrieves a set of all the values associated with the key within the given
;; syntax object and returns them as a list.
(define syntax-property-values
  (compose set->list syntax-property-values*))

;; Syntax Any Any ... → Syntax
;; Attaches several syntax properties to the syntax.
(define-syntax (syntax-properties stx)
  (syntax-parse stx
    [(_ x) #'x]
    [(_ x k v more ...)
     #'(syntax-property (syntax-properties x more ...) k v #t)]))

;; Syntax → Syntax
;; Tell SCV to ignore this piece of syntax.
(define (scv-ignore stx)
  (syntax-property stx 'scv:ignore #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; module syntax

;; Path-String → Syntax
;; Retrieves syntax object from a path.
(define (syntax-fetch target)
  (define port (open-input-file target))
  (port-count-lines! port)
  (begin0
    (with-module-reading-parameterization
      (thunk
       (read-syntax (object-name port) port)))
    (close-input-port port)))

;; Syntax → [Listof Module-Path]
;; Returns the list of modules that this piece of syntax depends on. We need
;; to explicitly require such modules in our elaborated syntax.
(define (syntax-deps stx)
  (set->list
   (let go ([e stx])
     (cond
       [(identifier? e)
        (or (and~> e
                   identifier-binding
                   third
                   module-path-index->module-path
                   set)
            (set))]
       [(syntax? e) (go (syntax-e e))]
       [(pair? e) (set-union (go (car e)) (go (cdr e)))]
       [else (set)]))))

;; Module-Path-Index → Module-Path
;; Converts module path index to a relative module path.
(define (module-path-index->module-path mpi)
  (define mp (collapse-module-path-index mpi))
  (or (and~> mp
             (satisfies path?)
             (simplify-path _ #f))
      mp))

;; Path-String → Boolean
;; Return whether target is a Typed Racket module.
(define (typed? target)
  (unless (file-exists? target)
    (error 'typed? "~a doesn't exist" target))
  (string-prefix? (lang-file-lang target) "typed/racket"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; etc

;; Syntax → Syntax
;; Protects the scopes on this syntax from erasure during `strip-context*`.
(define protect (λ~> (syntax-property 'protect-scope #t)))

;; Syntax → Boolean
;; Returns if the syntax has scopes protected.
(define protected? (λ~> (syntax-property 'protect-scope)))

;; Syntax {Syntax} → [Or #f Syntax]
;; Sets the parent identifier of a piece of syntax, or returns it.
(define (syntax-parent stx [parent #f])
  (if parent
      (syntax-property stx 'parent-identifier parent)
      (syntax-property stx 'parent-identifier)))

;; Syntax → Syntax
;; Strip context from syntax, unless there is a protect-scope syntax property.
(define (strip-context* stx)
  (let go ([e stx])
    (cond
      [(and (syntax? e) (protected? e)) e]
      [(syntax? e) (datum->syntax #f (go (syntax-e e)) e e)]
      [(pair? e) (cons (go (car e)) (go (cdr e)))]
      [else e])))

;; Syntax → [Or Syntax #f]
;; Returns non-lifted version of an identifier or #f if it was not lifted in the
;; first place.
(define (lifted->l stx)
  (and (identifier? stx)
       (match (~> stx syntax-e symbol->string)
         [(pregexp "^lifted/(\\d+)$" (list _ n))
          (format-id #f "l~a" n)]
         [else #f])))

;; Syntax Identifier → Boolean
;; Returns if a piece of syntax contains the given identifier.
(define (contains-id? stx given-id)
  (syntax-parse stx
    [(x ...)
     (ormap (λ~> (contains-id? given-id)) (attribute x))]
    [x:id
     (free-identifier=? #'x given-id)]
    [_ #f]))

;; Definitions Syntax → Syntax
;; Returns the co-domain of the given contract.
(define (chase-codomain definitions ctc)
  (let go ([ctc ctc])
    (syntax-parse (hash-ref definitions (syntax-e ctc))
      #:datum-literals (->)
      [x:id (go #'x)]
      [(-> _ ... x) #'x]
      [_ (error 'chase-codomain "~a isn't a function contract" ctc)])))

;; Syntax Syntax → Boolean
;; Returns whether two syntaxes have the same structure.
(define syntax-datum-equal? (syntax->datum . on . equal?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax classes

(define-syntax-class clause
  (pattern (~or x:id [x:id _])
           #:with (out ...) #'(x)
           #:with define #'(void))

  (pattern [#:opaque x pred]
           #:with (out ...) #'(x pred)
           #:with define #'(define-type x (Opaque pred)))

  (pattern [#:struct x:id ([f:id : t] ...)]
           #:with (out ...) #'((struct-out x))
           #:with define #'(void))

  (pattern [#:struct [x:id y:id] ([f:id : t] ...)]
           #:with (out ...) #'((struct-out x))
           #:with define #'(void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           racket/runtime-path
           "../test/path.rkt")

  (with-chk (['name "normalize-srcloc"])
    (define stx (normalize-srcloc (datum->syntax #f '(1 2 3)) ""))
    (match-define (list x y z) (syntax-e stx))
    (chk
     (syntax-column stx) 0
     (syntax-column x) 1
     (syntax-column y) 3
     (syntax-column z) 5))

  (with-chk (['name "syntax-property-values"])
    (define apple (syntax-property #'apple 'key 'apple))
    (define banana (syntax-property #'banana 'key 'banana))
    (define fruits (datum->syntax #f (list apple banana)))
    (chk #:eq set=? (syntax-property-values fruits 'key) '(apple banana)))


  (with-chk (['name "syntax-fetch"])
    (chk (syntax->datum (syntax-fetch untyped))
         '(module untyped racket/base
            (#%module-begin "hello world"))))

  (with-chk (['name "syntax-deps"])
    (define deps
      (syntax-deps
       (expand-syntax
        #'(module foo racket/base
            (require racket/math)
            pi))))
    (chk
     #:t (member '(lib "racket/math.rkt") deps)))

  (with-chk (['name "typed?"])
    (chk
     #:! #:t (typed? untyped)
     #:t (typed? typed)
     #:t (typed? also-typed)))

  (with-chk (['name "lifted->l"])
    (chk
     (syntax->datum (lifted->l #'lifted/1)) 'l1
     #:! #:t (lifted->l #'blah)))

  (with-chk (['name "protect, protected?"])
    (chk
     #:! #:t (protected? #'_)
     #:t (protected? (protect #'_))))

  (with-chk (['name "syntax-parent"])
    (chk
     (syntax-parent (syntax-parent #'_ 'foo))
     'foo))

  (with-chk (['name "strip-context*"])
    (chk
     #:! #:eq bound-identifier=? (strip-context* #'x) #'x
     #:eq bound-identifier=?
     (strip-context* (protect #'x))
     #'x))

  (with-chk (['name "contains-id?"])
    (chk
     #:t (contains-id? #'(+ 3 (add1 7)) #'add1)
     #:t (contains-id? #'(+ 3 (add1 x y)) #'x)
     #:! #:t (contains-id? #'(+ 3 (add1 7)) #'foo)))

  (with-chk (['name "chase-codomain"])
    (chk #:eq free-identifier=?
         (chase-codomain (hash 'a #'b
                               'b #'c
                               'c #'(-> x y z w))
                         #'a)
         #'w))

  (with-chk (['name "syntax-datum-equal?"])
    (chk
     #:t (syntax-datum-equal? #'(foo (bar baz))
                              (datum->syntax #f '(foo (bar baz))))
     #:! #:t (syntax-datum-equal? #'(foo (bar baz))
                                  #'(foo (bar baz qux)))))
  )