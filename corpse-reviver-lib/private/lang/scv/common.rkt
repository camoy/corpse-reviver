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
                     mischief/memoize
                     syntax/modresolve
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
    (define mod (hash-ref (current-mod-hash stx) path))
    (mod-contracts mod))

  ;; Syntax → Contracts
  ;; Given syntax from the currently expanding syntax, returns the module
  ;; paths under analysis.
  (define (current-mod-paths stx)
    (hash-keys (current-mod-hash stx)))

  ;; Syntax → [Hash String Mod]
  ;; Given syntax from the currently expanding syntax, returns a hash mapping
  ;; module paths to that module struct.
  (define (current-mod-hash stx)
    (define path (syntax-source stx))
    (continuation-mark-set-first (current-continuation-marks) 'mod-hash))

  ;; → [Listof String]
  ;; Returns the list of modules that should be treated as opaque.
  (define (current-opaques)
    (or (continuation-mark-set-first (current-continuation-marks) 'opaques)
        null))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require helpers

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
      (opaque-structs imports req-stx))
    (define single-defns
      (opaque-single-defns (set-subtract import-ids struct-imports) req-stx))
    (append struct-defns single-defns))

  ;; [Listof Identifier] Syntax → Syntax
  ;; Given a list of single import (i.e. non-struct) identifiers, returns syntax
  ;; defining them opaquely.
  (define (opaque-single-defns names req-stx)
    (for/list ([name (in-list names)])
      (datum->syntax req-stx `(define ,name #:opaque))))

  ;; [Listof Import] Syntax → [Listof Symbol] Syntax
  ;; Given a list of opaque imports, returns the identifiers defined by the
  ;; structs and also syntax defining the structs opaquely.
  (define (opaque-structs imports req-stx)
    (define structs (opaque-struct-names imports))
    (define names (map car structs))
    (define grouped-names (group-by cdr structs))
    (define struct-infos
      (for/append ([group (in-list grouped-names)])
        (define struct-names (map car structs))
        (define mod-name (cdr (car structs)))
        (dynamic-require-struct-infos mod-name struct-names)))
    (define sorted-struct-infos (sort-by-ancestry struct-infos))
    (values (struct-imports sorted-struct-infos)
            (struct-defns sorted-struct-infos names req-stx)))

  ;; [Listof Struct-Info] → [Listof Struct-Info]
  ;; Sort the list of struct infos based on struct ancestry.
  (define (sort-by-ancestry struct-infos)
    (define/for/fold ([struct-hash (hash)]
                      [parent-hash (hash)])
                     ([si (in-list struct-infos)])
      (match-define (list desc _ _ _ _ sup) si)
      (define name (descriptor->name desc))
      (values (hash-set struct-hash name si)
              (hash-set parent-hash name (list sup))))
    (define sorted-names (topological-sort parent-hash))
    (for/list ([name (in-list sorted-names)])
      (hash-ref struct-hash name)))

  ;; [Listof Struct-Info] → [Listof Symbol]
  ;; Given struct infos, returns all symbols defined by that struct definition.
  (define (struct-imports struct-infos)
    (for/append ([si (in-list struct-infos)])
      (match-define (list desc ctr pred accs muts _) si)
      (append (list desc (descriptor->name desc) ctr pred)
              accs
              (filter values muts))))

  ;; [Listof Struct-Info] [Listof Symbol] Syntax → Syntax
  ;; Given struct infos, returns the syntax for defining those structs.
  (define (struct-defns struct-infos names req-stx)
    (for/list ([si (in-list struct-infos)])
      (match-define (list desc ctr pred accs muts sup) si)
      (define name-string (substring (symbol->string desc) 7))
      (define name (string->symbol name-string))
      (define fld-prefix (format "~a-" name))
      (define flds
        (for/list ([acc (in-list (reverse (map symbol->string accs)))]
                   #:when (string-prefix? acc fld-prefix))
          (string->symbol (substring acc (string-length fld-prefix)))))
      (define sup? (and (symbol? sup) (member sup names)))
      (define mut? (ormap values muts))
      (define decl
        ((compose (λ (x) (append x (list '#:constructor-name ctr)))
                  (λ (x) (if mut? (append x (list '#:mutable)) x))
                  (λ (x) (append x (list flds)))
                  (λ (x) (if sup? (append x (list sup)) x)))
         `(racket:struct ,name)))
      (datum->syntax req-stx decl)))

  ;; [Listof Import] → [Listof [Cons Symbol String]]
  ;; Given a list of imports, returns the struct names that can be inferred
  ;; from the imports and the module they came from.
  (define (opaque-struct-names imports)
    (for/filter ([import (in-list imports)])
      (define module-name (~> import import-src-mod-path syntax-e))
      (and~> import
             import-local-id
             syntax-e
             descriptor->name
             (cons _ module-name))))

  ;; Symbol → [Or #f Symbol]
  ;; Given a symbol, returns the struct name associated with that descriptor or
  ;; #f if it isn't a descriptor.
  (define descriptor->name
    (λ-and~> symbol->string
             (satisfies (λ~> (string-prefix? "struct:")))
             (substring 7)
             string->symbol))

  ;; Syntax → [Listof Identifier]
  ;; Returns a list of opaque import identifiers.
  (define (opaque-import-ids req-stx)
    (map import-local-id (opaque-imports req-stx)))

  ;; Syntax → [Listof Import]
  ;; Returns a list of imports that come from opaque modules. We use the defining
  ;; module and not the nominal module (i.e. the one from `import-src-mod-path`)
  ;; since this is what SCV reports in its missing exception.
  (define/memoize (opaque-imports req-stx)
    (define opaque-mods (current-opaques))
    (define mod-paths (current-mod-paths req-stx))
    (define-values (imports srcs) (expand-import req-stx))
    (cond
      [(syntax-property req-stx 'opaque)
       (visit-import-sources! srcs)
       imports]
      [(not-opaque? opaque-mods mod-paths imports) null]
      [else
       (visit-import-sources! srcs)
       (filter (λ~> import->defining-module
                    (member opaque-mods))
               imports)]))

  ;; [Listof Import-Source] → Any
  ;; Visit the given import sources.
  (define (visit-import-sources! srcs)
    (for ([src (in-list srcs)])
      (~> src
          import-source-mod-path-stx
          syntax-e
          namespace-require/expansion-time)))

  ;; [Listof String] [Listof String] [Listof Import] → Boolean
  ;; Function that can shortcut checking if the given import is opaque. If there
  ;; are not opaque modules or if the require spec only imports modules being
  ;; analyzed so have no need to check.
  ;;
  ;; HACK: Remove when SCV #119 is resolved.
  (define (not-opaque? opaque-mods mod-paths imports)
    (define srcs
      (for/set ([import (in-list imports)])
        (define path (import-src-mod-path import))
        (define path* (if (syntax? path) (syntax-e path) path))
        (and~> path*
               resolve-module-path
               (satisfies path?)
               path->string)))
    (or (empty? opaque-mods)
        (for/and ([src (in-set srcs)])
          (member src mod-paths))))

  ;; Import → [Or #f String]
  ;; Given an import, returns the path where that import was originally defined
  ;; and not the nominal module path. Returns false if it fails to do so.
  (define (import->defining-module import)
    (define src-mod (~> import import-src-mod-path syntax-e))
    (define ns (make-namespace/memo src-mod))
    (define id
      (parameterize ([current-namespace ns])
        (namespace-symbol->identifier (import-src-sym import))))
    (define id-binding (identifier-binding id))
    (cond
      [id-binding
       (match-define (list defining-mod _ _ _ _ _ _) id-binding)
       (define resolved-mod (resolve-module-path-index defining-mod))
       (and (path? resolved-mod) (path->string resolved-mod))]
      [else #f]))

  ;; Any → Namespace
  ;; Constructs a namespace that uses makes the given require form's runtime
  ;; available at phase 1.
  (define/memoize (make-namespace/memo req)
    (define ns (make-base-namespace))
    (parameterize ([current-namespace ns])
      (namespace-require/expansion-time `(for-template ,req))
      ns))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require macro

;; Need to check require forms for imports from opaque modules. If there are
;; imports from an opaque module, we need to filter them out and redefine them
;; as opaque.
(define-syntax (-require stx)
  (syntax-parse stx
    [(_ ?x ...)
     #:with (?x* ...) (map subtract-opaques (syntax->list #'(?x ...)))
     #:with (?def ...) (append-map opaque-defns (syntax->list #'(?x ...)))
     #'(begin (require ?x* ...) ?def ...)]))
