#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (for-syntax current-contracts
                     current-opaques)
         -require)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/require-transform
                     racket/list
                     racket/match
                     racket/set
                     racket/string
                     mischief/for
                     mischief/module
                     syntax/parse
                     syntax/strip-context
                     threading
                     "../../data.rkt"
                     "../../util.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; getters

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
    (values (struct-imports struct-infos)
            (struct-defns struct-infos)))

  ;; [Listof Struct-Info] → [Listof Symbol]
  ;; Given struct infos, returns all symbols defined by that struct definition.
  (define (struct-imports struct-infos)
    (for/append ([si (in-list struct-infos)])
      (match-define (list desc ctr pred accs muts _) si)
      (append (list desc ctr pred)
              accs
              (filter values muts))))

  ;; [Listof Struct-Info] → Syntax
  ;; Given struct infos, returns the syntax for defining those structs.
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

  ;; Any [Listof Symbol] → [Listof Struct-Info]
  ;; Given a quoted raw require spec and a list of struct names, returns the
  ;; (extracted as a list) struct infos for the structs.
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
     #'(begin (require ?x* ...) ?def ...)]))

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
