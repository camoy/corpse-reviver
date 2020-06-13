#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (rename-out [-require require])
         require/typed
         require/typed/provide

         (rename-out [-provide provide])
         (rename-out [-provide scv-cr:provide])

         (rename-out [struct racket:struct])
         define-predicate
         make-predicate
         require/define)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/function
                     racket/list
                     racket/match
                     racket/set
                     racket/struct-info
                     racket/string
                     racket/syntax
                     racket/require-transform
                     mischief/for
                     mischief/module
                     soft-contract/parse/utils
                     syntax/parse
                     syntax/strip-context
                     threading
                     "../../syntax.rkt"
                     "../../struct.rkt"
                     "../../data.rkt"
                     "../../util.rkt")
         (only-in soft-contract/fake-contract [provide scv:provide]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic utils

(begin-for-syntax
  ;; Syntax → Contracts
  ;; Given syntax from the currently expanding syntax, returns the contracts
  ;; struct for that syntax.
  (define (current-contracts stx)
    (define path (syntax-source stx))
    (define mod-hash
      (continuation-mark-set-first (current-continuation-marks) 'mod-hash))
    (define mod (hash-ref mod-hash path))
    (mod-contracts mod))

  ;; → [Listof String]
  ;; Returns the list of modules that should be treated as opaque.
  (define (current-opaques)
    (or (continuation-mark-set-first (current-continuation-marks) 'opaques)
        null))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require macros

(begin-for-syntax
  ;; Syntax → Syntax
  ;; Returns a form that subtracts out all identifiers that come from opaque
  ;; modules.
  (define (subtract-opaques req-stx)
    #`(except-in #,req-stx #,@(opaque-import-ids req-stx)))

  ;; Syntax → [Listof Syntax]
  ;; Returns syntax that defines all the identifiers that came from opaque
  ;; modules.
  (define (opaque-defns req-stx)
    (define imports (opaque-imports req-stx))
    (define import-ids (map (λ~> import-local-id syntax-e) imports))
    (define-values (struct-imports struct-defns)
      (opaque-structs imports))
    (define single-defns
      (opaque-single-defns (set-subtract import-ids struct-imports)))
    (append struct-defns single-defns))

  ;; [Listof Identifier] → Syntax
  ;; Given a list of single import (i.e. non-struct) identifiers, returns syntax
  ;; defining them opaquely.
  (define (opaque-single-defns names)
    (for/list ([name (in-list names)])
      (datum->syntax #f `(define ,name #:opaque))))

  ;; [Listof Import] → [Listof Symbol] Syntax
  ;; Given a list of opaque imports, returns the identifiers defined by the
  ;; structs and also syntax defining the structs opaquely.
  (define (opaque-structs imports)
    (define structs (opaque-struct-names imports))
    (define grouped-names (group-by cdr structs))
    (define struct-infos
      (for/append ([group (in-list grouped-names)])
        (define struct-names (map car structs))
        (define mod-name (cdr (car structs)))
        (dynamic-require-struct-infos mod-name struct-names)))
    (values (struct-exports struct-infos)
            (struct-defns struct-infos)))

  ;;
  ;; TODO
  (define (struct-exports struct-infos)
    (for/append ([si (in-list struct-infos)])
      (match-define (list desc ctr pred accs muts _) si)
      (append (list desc ctr pred)
              accs
              (filter values muts))))

  ;;
  ;; TODO
  (define (struct-defns struct-infos)
    (for/list ([si (in-list struct-infos)])
      (match-define (list desc ctr pred accs muts sup) si)
      (define name-string (substring (symbol->string desc) 7))
      (define name (string->symbol name-string))
      (define fld-prefix (format "~a-" name))
      (define flds
        (for/list ([acc (in-list (reverse (map symbol->string accs)))]
                   #:when (string-prefix? acc fld-prefix))
          (string->symbol (substring acc (string-length fld-prefix)))))
      (define sup? (symbol? sup))
      (define mut? (ormap values muts))
      (define decl
        ((compose (λ (x) (append x (list '#:constructor-name ctr)))
                  (λ (x) (if mut? (append x (list '#:mutable)) x))
                  (λ (x) (append x (list flds)))
                  (λ (x) (if sup? (append x (list sup)) x)))
         `(racket:struct ,name)))
      (datum->syntax #f decl)))

  ;; [Listof Import] → [Listof [Cons Symbol String]]
  ;; Given a list of imports, returns the struct names that can be inferred
  ;; from the imports and the module they came from.
  (define (opaque-struct-names imports)
    (define (struct-name? x)
      (string-prefix? x "struct:"))
    (for/filter ([import (in-list imports)])
      (define module-name (~> import import-src-mod-path syntax-e))
      (and~> import
             import-local-id
             syntax-e
             symbol->string
             (satisfies struct-name? _)
             (substring 7)
             string->symbol
             (cons _ module-name))))

  ;; Syntax → [Listof Identifier]
  ;; Returns a list of opaque import identifiers.
  (define (opaque-import-ids req-stx)
    (map import-local-id (opaque-imports req-stx)))

  ;; Syntax → [Listof Import]
  ;; Returns a list of imports that come from opaque modules.
  (define (opaque-imports req-stx)
    (define opaque-mods (current-opaques))
    (define-values (imports _) (expand-import req-stx))
    (filter (λ~> import-src-mod-path
                 syntax-e
                 resolve-module-path
                 resolved-module-path-name
                 path->string
                 (member opaque-mods))
            imports))

  ;;
  ;; TODO
  (define (dynamic-require-struct-infos module bindings)
    (define ns (make-base-namespace))
    (parameterize ([current-namespace ns])
      (namespace-require '(for-syntax racket/base racket/struct-info))
      (namespace-require/expansion-time module)
      (eval `(define-syntax (cheater-x stx)
               #`'#,(for/list ([binding (in-list ',bindings)])
                      (extract-struct-info (syntax-local-value #`#,binding)))))
      (eval 'cheater-x)))
  )

;; Need to check require forms for imports from opaque modules. If there are
;; imports from an opaque module, we need to filter them out and redefine them
;; as opaque.
(define-syntax (-require stx)
  (syntax-parse stx
    [(_ ?x ...)
     #:with (?x* ...) (map subtract-opaques (syntax->list #'(?x ...)))
     #:with (?def ...) (append-map (λ~>> opaque-defns
                                        (replace-context stx))
                                   (syntax->list #'(?x ...)))
     (log-warning (format "~a" #'(begin (require ?x* ...) ?def ...)))
     #;(replace-context stx #'(begin (racket:require ?x* ...) ?def ...))
     #'(begin (require ?x* ...) ?def ...)]))

;; Disable require/typed within an analysis (since this will always be provided
;; by SCV-CR via a require/safe submodule). The only exception are clauses marked
;; with opaque. These are not imported so we have to define them as opaque types.
(define-syntax (require/typed stx)
  (syntax-parse stx
    [(_ ?m:expr ?x:clause ...)
     (replace-context stx #'(begin ?x.define ...))]))

;; Disable require/typed/provide within an analysis for the same reason as above,
;; but provide the bindings since Typed Racket will not give those to the provide
;; bundle on a require/typed/provide.
(define-syntax (require/typed/provide stx)
  (syntax-parse stx
    [(_ ?m:expr ?x:clause ...)
     (define provides (with-syntax-source stx #'(provide ?x.out ... ...)))
     (replace-context stx #`(begin ?x.define ... #,provides))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide macro

(begin-for-syntax
  ;; Syntax [Listof Syntax] → Boolean
  ;; Takes in the forms of a provide and determines if they should be excluded
  ;; (since they have already been exported by SCV-CR).
  (define (exclude-outs stx xs)
    (define bundle (contracts-provide (current-contracts stx)))
    (define exports (bundle-exports bundle))
    (define structs (bundle-structs bundle))
    (define struct-names (hash-keys structs))
    (define struct-exports (structs-exports structs))
    (define (should-exclude? form)
      (match (syntax->datum form)
        [`(struct-out ,name) (member name struct-names)]
        [`(contract-out ,out ...) #f]
        [`(f:contract-out ,out ...) #f]
        [(? symbol? x) (or (hash-has-key? exports x)
                           (member x struct-exports))]
        [x (error 'exclude-outs "unrecognized provide form ~a" x)]))
    (filter (negate should-exclude?) xs))
  )

;; Provide but excluding identifiers that were already exported by SCV-CR with
;; contracts.
(define-syntax (-provide stx)
  (syntax-parse stx
    [(_ ?x ...)
     #:with (?x* ...) (exclude-outs stx (attribute ?x))
     #'(scv:provide ?x* ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predicate macros

(begin-for-syntax
  ;; Syntax → Syntax
  ;; Get predicate from syntax.
  (define (get-predicate stx)
    (replace-context
     stx
     (hash-ref (contracts-predicates (current-contracts stx))
               (syntax->datum stx))))
  )

;; Defines a predicate based on a type.
(define-syntax (define-predicate stx)
  (syntax-parse stx
    [(_ ?x:id _)
     #:with ?defn (get-predicate stx)
     #'(define ?x ?defn)]))

;; Returns a predicate based on a type.
(define-syntax make-predicate get-predicate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; redefinition
;;
;; Suppose a module imports an identifier, say a predicate that defines an opaque
;; type. Use of that opaque type in other modules will point the predicate's
;; binding to that module. However, when we do elaboration, that module's imports
;; will be pushed into a require/contracts submodule, invalidating that
;; reference. To solve this we re-define single (non-struct) imports in the
;; current module if it was imported from require/contracts. Some scope nonsense
;; is necessary to prevent this from clashing with the existing import of
;; require/typed.

(begin-for-syntax
  ;; Scope for redefinition import.
  (define sc (make-syntax-introducer))

  ;; Syntax → Syntax
  ;; Get the name of a predicate.
  (define (predicate-name id)
    (format-id id "~a?" id))

  ;; Syntax → Syntax
  ;; Get the real predicate identifier.
  (define (predicate-id id)
    (third (extract-struct-info (syntax-local-value (sc id))))))

;; Syntax → Syntax
;; Require, but redefine the given imports and struct exports in this module.
(define-syntax (require/define stx)
  (syntax-parse stx
    [(_ m (imp:id ...) (s-imp:id ...))
     #:with [s-imp? ...] (map predicate-name (attribute s-imp))
     #:with m* (sc #'m)
     #'(begin
         (require (only-in m* imp ... s-imp? ...)
                  (except-in m imp ... s-imp? ...))
         (redefine (imp ...) (s-imp ...)))]))

;; Syntax → Syntax
;; Do the actual redefinition (this time we have the struct info).
(define-syntax (redefine stx)
  (syntax-parse stx
    [(_ (imp:id ...) (s-imp:id ...))
     #:with [s-imp? ...] (map predicate-name (attribute s-imp))
     #:with [s-imp?* ...] (map (compose sc predicate-id) (attribute s-imp))
     #:with [imp* ...] (map sc (attribute imp))
     #'(define-values (imp ... s-imp? ...)
         (values imp* ... s-imp?* ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(begin-for-syntax
  (module+ test
    (require chk)

    (with-chk (['name "opaque-struct-names"])
      (chk
       (map syntax-e
            (opaque-struct-names (list #'struct:foo #'struct:bar #'hello?)))
       '(foo bar)))))
