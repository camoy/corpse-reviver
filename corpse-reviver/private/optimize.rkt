#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [optimize (-> (listof mod?) (listof mod?))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require fancy-app
         graph
         mischief/for
         racket/function
         racket/hash
         racket/list
         racket/match
         racket/math
         racket/struct
         soft-contract/main
         syntax/parse
         syntax/strip-context
         threading
         "compile.rkt"
         "data.rkt"
         "elaborate.rkt"
         "struct.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; optimize

;; [Listof Mod] → [Listof Mod]
;; Optimizes the given modules by bypassing contracts on require/typed forms that
;; are proven safe and by creating an unsafe submodule that untyped modules
;; which are proven safe can use. We ignore all blame of a typed module (these
;; are known false positives by Typed Racket's soundness).
(define (optimize -mods)
  (define mods (map (elaborate _ #f) -mods))
  (define scv-mods (map (elaborate _ #t) -mods))

  ;; STOP! This is always needed (and I've wasted many hours debugging by
  ;; forgetting it). Without compiling modules with the elaborated source, SCV
  ;; will give mysterious missing identifier errors.
  (compile-modules (filter mod-typed? mods))
  (define/for/lists (targets stxs)
    ([mod (in-list scv-mods)])
    (values (mod-target mod) (mod-syntax mod)))
  (define -blms
    (with-patched-typed-racket
      (verify-modules targets stxs)))
  (define blms (filter (untyped-blame? mods) -blms))

  (for/list ([mod (in-list mods)])
    (define blms-mod
      (filter (if (mod-typed? mod)
                  (blame-violates-my-contract? mod)
                  (blame-me? mod))
              blms))
    (define unsafe-hash (unsafe mod mods blms-mod))
    (optimize-mod mod unsafe-hash)))

;; Mod [Hash Complete-Path Symbol] → Mod
;; Optimize a module by attaching metadata to direct bypassing contracts on safe
;; imports.
(define (optimize-mod m unsafe-hash)
  (define stx*
    (syntax-parse (mod-raw m)
      [(module ?name ?lang (?mb ?body ...))
       #:with ?lang* (optimize-lang #'?lang)
       (strip-context
        #`(module ?name ?lang*
            (register-unsafe-hash! #,unsafe-hash)
            ?body ...))]))
  (struct-copy mod m [syntax stx*]))

;; Syntax → Syntax
;; Returns the name of the SCV-CR language for the new module.
(define (optimize-lang lang)
  (match (syntax-e lang)
    ['racket/base #'corpse-reviver/private/lang/untyped/base]
    ['racket #'corpse-reviver/private/lang/untyped/full]
    ['typed/racket/base #'corpse-reviver/private/lang/typed/base]
    ['typed/racket #'corpse-reviver/private/lang/typed/full]
    [else (error 'untyped-lang "unknown language ~a" lang)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unsafe

;; Mod [Listof Mod] [Listof Blame] → [Hash Complete-Path [Listof Symbol]]
;; Returns a hash that maps modules to a list of bindings that are unsafe and
;; must be imported with contracts enabled.
(define (unsafe mod mods blms)
  (define (server-module blm)
    (define path (blame-server blm))
    (findf (equal? (mod-target _) path) mods))
  (define unsafe
    (for/list ([blm (in-list blms)]
               #:when (server-module blm))
      (define mod (server-module blm))
      (define bundle (contracts-provide (mod-contracts mod)))
      (define residuals (residual-contracts mod blm))
      (hash (blame-server blm) (bundle->unsafe-exports bundle residuals))))
  (define default-hash
    (for/hash ([mod (in-list mods)]
               #:when (mod-typed? mod))
      (values (mod-target mod) null)))
  (apply hash-union #:combine append default-hash unsafe))

;; Bundle [Listof Symbol] → [Listof Symbol]
;; Returns a list of unsafe exports given a list of residual contracts.
(define (bundle->unsafe-exports bundle residuals)
  (filter (member _ residuals)
          (append (hash-keys (bundle-exports bundle))
                  (structs-exports (bundle-structs bundle)))))

;; Mod Blame → [Listof Symbol]
;; Returns a list of contract identifiers that need to be kept according to the
;; blames given in the blms.
(define (residual-contracts mod blm)
  (define positions (mod-positions mod))
  (define deps (mod-deps mod))
  (define-values (line col) (blame-contract-position blm))
  (define contract-to-blame (hash-ref positions (cons line col)))
  (define-values (id->dist _) (bfs deps contract-to-blame))
  (for/list ([(id dist) (in-hash id->dist)]
             #:when (not (infinite? dist)))
    id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; blame

;; [Listof Mod] → (Blame → Boolean)
(define (untyped-blame? mods)
  (let ([typed (filter-map typed-module-target mods)])
    (λ (blm)
      (not (member (blame-violator blm) typed)))))

;; Mod → Path-String
(define typed-module-target
  (λ-and~>> (satisfies mod-typed?) mod-target))

;; Blame Mod → Boolean
(define ((blame-me? me) blm)
  (equal? (blame-violator blm) (mod-target me)))

;; Blame Mod → Boolean
(define ((blame-violates-my-contract? me) blm)
  (equal? (blame-server blm) (mod-target me)))

;; Blame → Path-String
(define (blame-violator blm)
  (match blm
    [(list violator _ _ _ _) violator]))

;; Blame → Path-String
(define (blame-server blm)
  (match blm
    [(list _ _ (list server _ _) _ _) server]))

;; Blame → Integer Integer
(define (blame-contract-position blm)
  (match blm
    [(list _ _ (list _ line col) _ _)
     (values line col)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk
           racket/pretty
           "elaborate.rkt"
           "../test/mod.rkt")

  (define ty-ty (list ty-ty-server-mod ty-ty-client-mod))
  (optimize ty-ty)
  )
