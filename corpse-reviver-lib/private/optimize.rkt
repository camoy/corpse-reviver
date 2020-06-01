#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [optimize (-> (listof mod?) (listof mod?))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require graph
         mischief/for
         racket/function
         racket/hash
         racket/list
         racket/match
         racket/math
         racket/pretty
         racket/set
         racket/struct
         soft-contract/main
         syntax/parse
         syntax/strip-context
         threading
         "compile.rkt"
         "data.rkt"
         "elaborate.rkt"
         "logging.rkt"
         "struct.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; optimize

;; [Listof Mod] → [Listof Mod]
;; Optimizes the given modules by bypassing contracts on require/typed forms that
;; are proven safe and by creating an unsafe submodule that untyped modules
;; which are proven safe can use. We ignore all blame of a typed module (these
;; are known false positives by Typed Racket's soundness).
(define (optimize mods)
  (debug "optimize: ~a" (pretty-format mods))

  ;; STOP! This is always needed (and I've wasted many hours debugging by
  ;; forgetting it). Without compiling modules with the elaborated source, SCV
  ;; will give mysterious missing identifier errors.
  (compile-modules mods)

  ;; Run SCV
  (define/for/lists (targets stxs)
    ([mod (in-list mods)])
    (values (mod-target mod) (mod-syntax mod)))
  (debug "targets: ~a" targets)
  (define -blms
    (measure 'analyze
      (with-continuation-mark 'scv? #t
        ;; HACK: We need this only for the benchmark-dependent patches.
        ;; Remove this once TR #837 is resolved.
        (with-patched-typed-racket
          (λ () (verify-modules targets stxs))))))
  (info 'blame -blms)
  (define blms (filter (untyped-blame? mods) -blms))
  (debug "filtered analysis: ~a" -blms)

  ;; Optimize with analysis results
  (for/list ([mod (in-list mods)])
    (define blame-filter
      (if (mod-typed? mod) blame-violates-my-contract? blame-me?))
    (define blms-mod (filter blame-filter blms))
    (define unsafe-hash (unsafe mods blms-mod))
    (optimize+unsafe mod unsafe-hash)))

;; Mod [Hash Complete-Path Symbol] → Mod
;; Optimize a module by attaching metadata to direct bypassing contracts on safe
;; imports.
(define (optimize+unsafe m unsafe-hash)
  (define stx
    (syntax-parse (mod-raw m)
      #:datum-literals (module #%module-begin)
      [(module ?name ?lang (#%module-begin ?body ...))
       #:with ?lang* (optimize-lang #'?lang)
       (strip-context
        #`(module ?name ?lang*
            (#%module-begin
             (register-unsafe-hash! #,unsafe-hash)
             ?body ...)))]))
  (debug "optimized ~a:\n~a" (mod-target m) stx)
  (struct-copy mod m [syntax stx]))

;; Syntax → Syntax
;; Returns the name of the SCV-CR language for the new module.
(define (optimize-lang lang)
  (match (syntax-e lang)
    ['racket/base #'corpse-reviver/private/lang/untyped/base]
    ['racket #'corpse-reviver/private/lang/untyped/full]
    ['typed/racket/base #'corpse-reviver/private/lang/typed/base]
    ['typed/racket #'corpse-reviver/private/lang/typed/full]
    [else (error 'optimize-lang "unknown language ~a" lang)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unsafe

;; [Listof Mod] [Listof Blame] → [Hash Complete-Path [Listof Symbol]]
;; Returns a hash that maps modules to a list of bindings that are unsafe and
;; must be imported with contracts enabled.
(define (unsafe mods blms)
  (define (server-module blm)
    (define path (blame-server blm))
    (define result (findf (λ~> mod-target (equal? path)) mods))
    (when (not result)
      (warn (format "cannot locate contract for blame: ~a" (pretty-format blm))))
    result)
  (define unsafe
    (for/list ([blm (in-list blms)]
               #:when (server-module blm))
      (define mod (server-module blm))
      (define bundle (contracts-provide (mod-contracts mod)))
      (define residuals (residual-contracts mod blm))
      (hash (blame-server blm) (bundle->unsafe-exports bundle residuals))))
  (define default-hash
    (for/hash ([mod (in-list mods)])
      (values (mod-target mod) null)))
  (apply hash-union #:combine append default-hash unsafe))

;; Bundle [Listof Symbol] → [Listof Symbol]
;; Returns a list of unsafe exports given a list of residual contracts. If
;; residuals has a struct name, we take all struct exports. Otherwise, we
;; simply take the identifier itself if it's an export.
(define (bundle->unsafe-exports bundle residuals)
  (define exports (hash-keys (bundle-exports bundle)))
  (for/fold ([unsafe null])
            ([id (in-list residuals)])
    (define struct-data (hash-ref (bundle-structs bundle) id (λ () #f)))
    (cond
      [struct-data
       (define struct-provides (struct-data-exports id struct-data))
       (set-union unsafe (set-intersect struct-provides exports))]
      [(member id exports) (set-add unsafe id)]
      [else unsafe])))

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
           racket/list
           racket/pretty
           racket/port
           racket/unsafe/ops
           rackunit
           "elaborate.rkt"
           "../test/path.rkt"
           "../test/mod.rkt")

  (cleanup-bytecode)

  (with-chk (['name "optimize (slow sieve)"])
    (define mods (optimize (list sieve-slow-streams-mod
                                 sieve-slow-main-mod)))
    (define old-times
      (parameterize ([current-namespace (make-base-namespace)])
        (with-output-to-string
          (λ ()
            (dynamic-require (string->path sieve-slow-main) #f)))))

    (compile-modules mods)

    (define new-times
      (parameterize ([current-namespace (make-base-namespace)])
        (with-output-to-string
          (λ ()
            (dynamic-require (string->path sieve-slow-main) #f)))))

    (define (real-time x)
      (string->number (second (regexp-match #px"real time: (\\d+)" x))))

    ;; Conservatively, the speedup from optimization should be at least 5x.
    (define speedup (/ (real-time old-times) (real-time new-times)))
    (chk
     #:t (> speedup 5)))

   #;(with-chk (['name "optimize (client and server)"])
     (define (chk-optimize server client)
       (define mods (optimize (list server client)))
       (compile-modules mods)
       (define g
         (parameterize ([current-namespace (make-base-namespace)])
           (dynamic-require (string->path (mod-target client)) 'g)))
       (chk (unsafe-struct-ref (g 42) 0) 42))
     (chk-optimize ty-ty-server-mod ty-ty-client-mod)
     (chk-optimize ty-ut-server-mod ty-ut-client-mod)
     (chk-optimize ut-ty-server-mod ut-ty-client-mod)
     (chk-optimize ut-ut-server-mod ut-ut-client-mod))
  )
