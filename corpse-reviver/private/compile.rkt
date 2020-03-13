#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [compile-modules (-> (listof mod?) any)]
  [compile+write/dir (-> path-string? syntax? any)]
  [expand/dir (-> path-string? syntax? syntax?)]

  [sort-by-dep (-> (listof mod?) (listof mod?))]

  [imports (-> path-string? (listof symbol?))]
  [delete-bytecode (-> path-string? any)])

 in-dir
 with-patched-typed-racket)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require compiler/compilation-path
         mischief/for
         racket/contract
         racket/file
         racket/function
         racket/list
         racket/match
         racket/path
         racket/runtime-path
         syntax/modcode
         syntax/modcollapse
         syntax/modresolve
         threading
         "data.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compilation and expansion

;; [Listof Mod] → Any
;; Compiles the given elaborated modules.
(define (compile-modules mods)
  (for ([mod (in-list mods)])
    (compile+write/dir (mod-target mod) (mod-syntax mod))))

;; Path-String Syntax → Any
;; Compiles syntax and writes bytecode to the filesystem.
(define (compile+write/dir target stx)
  (define zo (get-bytecode-file target))
  (make-parent-directory* zo)
  (with-output-to-file zo
    #:exists 'replace
    (thunk (write (compile/dir target stx)))))

;; Path-String Syntax → Compiled-Expression
;; Compiles syntax in the directory of the given path.
(define (compile/dir target stx)
  (in-dir target
    (parameterize ([current-namespace (make-base-namespace)])
      (compile stx))))

;; Path-String Syntax → Syntax
;; Expands syntax in the directory of the given path, with a patched Typed
;; Racket. We use the compiled-load handler for monkey patching (i.e. some Typed
;; Racket modules are redirected to our implementation).
(define (expand/dir target stx)
  (in-dir target
    (with-patched-typed-racket
      (expand stx))))

;; Executes the given expressions with an environment that has a patched Typed
;; Racket.
(define-syntax-rule (with-patched-typed-racket ?x ...)
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-load/use-compiled expand-load])
    ?x ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiled-load handler

;; Path to local Typed Racket patches.
(define-runtime-path CWD ".")
(define local-typed-racket (build-path CWD "typed-racket"))

;; Compiled-Load-Handler
;; Cache the old compiled-load handler for proxying.
(define old-load (current-load/use-compiled))

;; [Hash Symbol String] → [Hash Path Complete-Path]
;; Take the proxy hash and resolve paths as needed.
(define (resolve-proxy proxy-hash)
  (for/hash ([(k v) (in-hash proxy-hash)])
    (values (resolve-module-path k)
            (simple-form-path (build-path local-typed-racket v)))))

;; [Hash Symbol String]
;; Maps Typed Racket modules to locally patched modules for loading during
;; expansion.
(define proxy-hash
  #hash((typed-racket/base-env/prims-contract . "prims-contract.rkt")
        (typed-racket/base-env/prims . "prims.rkt")
        (typed-racket/private/type-contract . "type-contract.rkt")
        (typed-racket/static-contracts/combinators/function . "function.rkt")
        (typed-racket/static-contracts/instantiate . "instantiate.rkt")
        (typed-racket/typecheck/provide-handling . "provide-handling.rkt")
        (typed-racket/utils/require-contract . "require-contract.rkt")))

;; [Hash Path Path]
;; Prepare the proxy hash for use in the compiled-load handler by resolving
;; modules and completing paths.
(define resolved-proxy-hash (resolve-proxy proxy-hash))

;; Path Path → Any
;; The proxy compiled-load handler that uses the proxy hash to redirect
;; instantiations of Typed Racket modules to our local implementation.
(define (expand-load path name)
  (define path* (hash-ref resolved-proxy-hash path (const path)))
  (old-load path* name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sort by dependency

;; [Listof Mod] → [Listof Mod]
;; Sorts the list of modules according to dependency.
(define (sort-by-dep mods)
  (define dep-graph
    (for/hash ([mod (in-list mods)])
      (values (path->symbol (mod-target mod))
              (mod-imports mod))))
  (define targets (topological-sort dep-graph))
  (sort mods (⊑/dep targets)))

;; [Listof Symbol] → (Mod Mod → Boolean)
;; Comparator for modules based on dependency determined by the targets list.
(define (⊑/dep targets)
  (define rank-of
    (λ~>> mod-target path->symbol (index-of targets)))
  (rank-of . on . <))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; etc

;; Path-String → [Listof Symbol]
;; Returns the list of a module's local dependencies as symbols.
(define (imports target)
  (match-define (cons _ mpis)
    (assoc 0 (module-compiled-imports (get-module-code target))))
  (for/filter ([mpi (in-list mpis)])
    (and~> mpi
           collapse-module-path-index
           (resolve-module-path _ target)
           (satisfies path? _)
           path->symbol)))

;; Path-String → Any
;; Delete bytecode associated with target if it exists.
(define (delete-bytecode target)
  (define zo (get-bytecode-file target))
  (when (file-exists? zo)
    (delete-file zo)))

;; Path-String → Path-String
;; Returns the location of the target's bytecode.
(define (get-bytecode-file target)
  (get-compilation-bytecode-file target #:modes '("compiled")))

;; Evaluates with the current directory from the parent of the target.
(define-syntax-rule (in-dir target forms ...)
  (let ([parent (path-only target)])
    (parameterize ([current-directory parent]
                   [current-load-relative-directory parent])
      forms ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require (for-syntax racket/base
                       racket/syntax)
           (only-in rackunit/chk chk)
           rackunit
           syntax/parse/define
           "syntax.rkt"
           "test-util.rkt")

  (define goodbye-stx
    #'(module hello-world racket/base
        (provide str)
        (define str "goodbye world")))

  (define (make-mod target [stx #'_])
    (mod target #'_ stx #f #f (imports target) #f #f))

  (define-simple-macro (define-expand-provide ?x:id ...)
    #:with [?y ...] (map (λ (x) (format-id x "~a-expand" x)) (attribute ?x))
    (begin
      (begin
        (provide ?y)
        (define ?y (expand/dir ?x (syntax-fetch ?x))))
      ...))

  (define-expand-provide
    predicate
    ty-ty-client
    ty-ty-server
    ty-ut-client
    ty-ut-server
    ut-ty-client
    ut-ty-server
    ut-ut-client
    ut-ut-server
    sieve-main
    streams)

  (test-case "compile-modules"
    (after
     (compile-modules (list (make-mod hello-world goodbye-stx)))
     (chk
      (dynamic-require hello-world 'str)
      "goodbye world")
     (delete-bytecode hello-world)))

  (test-case "compile+write/dir"
    (after
     (compile+write/dir hello-world goodbye-stx)
     (chk
      (dynamic-require hello-world 'str)
      "goodbye world")
     (delete-bytecode hello-world)))

  (test-case "expand/dir"
    (define stx
      #'(module foo typed/racket/base
          (provide x)
          (: x Integer)
          (define x 7)))
    (define expanded-stx
      (expand/dir (path->complete-path ".") stx))
    (define provide-prop
      (car (syntax-property-values expanded-stx 'provide)))
    (chk
     #:t (syntax-case provide-prop
             (begin exact-integer? or/c flat-named-contract
              define-module-boundary-contract define define-values)
           [(begin (define _ (flat-named-contract _ exact-integer?))
                   (define _ (or/c _))
                   (define-values (_) _)
                   (define-module-boundary-contract _ ...))
            #t]
           [_ #f])))

  (test-case "sort-by-dep"
    (define-values (a-mod b-mod c-mod)
      (values (make-mod a) (make-mod b) (make-mod c)))
    (chk
     (sort-by-dep (list a-mod c-mod b-mod))
     (list c-mod b-mod a-mod)))

  (test-case "imports"
    (chk
     #:t (member (path->symbol (resolve-module-path 'racket/string))
                 (imports hello-world))))

  (test-case "delete-bytecode"
    (define zo (get-bytecode-file hello-world))
    (make-parent-directory* zo)
    (with-output-to-file zo
      #:exists 'replace
      (thunk (displayln "")))
    (delete-bytecode hello-world)
    (chk #:f #:t (file-exists? zo)))

  (test-case "get-bytecode-file"
    (chk
     (simple-form-path (get-bytecode-file untyped))
     (simple-form-path (build-path TEST-DIR "compiled" "untyped_rkt.zo"))))
  )
