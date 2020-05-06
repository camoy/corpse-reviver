#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [make-mod (-> path-string? mod?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require mischief/for
         racket/contract
         racket/list
         racket/match
         racket/set
         syntax/parse
         threading
         "compile.rkt"
         "data.rkt"
         "dependency.rkt"
         "extract.rkt"
         "log.rkt"
         "struct.rkt"
         "syntax.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mod

;; Path-String → Mod
;; Returns a module from a file and syntax.
(define (make-mod target)
  (debug "Begin (make-mod ~a)." target)
  (define raw-stx (syntax-fetch target))
  (if (typed? target)
      (typed->mod target raw-stx)
      (untyped->mod target raw-stx)))

;; Path-String Syntax → Mod
;; Returns module for a typed file from raw syntax.
(define (typed->mod target raw-stx)
  (define expanded-stx (expand/dir target raw-stx))
  (define ctcs (make-contracts expanded-stx))
  (define elaborated (elaborate raw-stx ctcs target))
  (debug "Elaborated (~a):\n ~a." target elaborated)
  (compile+write/dir target expanded-stx)
  (mod target
       raw-stx elaborated ctcs #t (imports target)
       (contract-positions elaborated)
       (contract-dependency elaborated)))

;; Path-String Syntax → Module
;; Returns module for an untyped file from raw syntax. This does nothing since
;; untyped code needs no elaboration.
(define (untyped->mod target raw-stx)
  (compile+write/dir target raw-stx)
  (mod target raw-stx raw-stx #f #f (imports target) #f #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elaborate

;; Syntax Contracts Path-String → Syntax
;; Elaborates syntax according to their contracts.
(define (elaborate raw-stx ctcs target)
  (define prov-bundle (contracts-provide ctcs))
  (define stx
    (syntax-parse raw-stx
      #:datum-literals (module)
      [(module ?name ?lang (?mb ?body ...))
       #:with ?lang/nc (no-check #'?lang)
       #:with ?sneak (syntax-property #''sneak 'payload ctcs)
       #:with ?prov (provide-inject prov-bundle)
       #:with ?req  (require-inject ctcs #'?lang/nc)
       #`(module ?name ?lang/nc
           (module #%type-decl racket/base)
           (register-contracts! ?sneak)
           ?prov ?req ?body ...)]))
  (~> stx
      hide-provide
      strip-context*
      contract-sc
      (normalize-srcloc target)))

;; Syntax Boolean → Syntax
;; Returns the name of the no-check language for the new module.
(define (no-check lang)
  (match (syntax-e lang)
    ['typed/racket/base #'corpse-reviver/private/lang/scv/base]
    ['typed/racket #'corpse-reviver/private/lang/scv/full]
    [else (error 'no-check "unknown language ~a" lang)]))

;; Syntax → Syntax
;; Hide provide forms. This is necessary because SCV directly looks at forms
;; labeled as provide. We need a level of indirection (scv-cr:provide) to get
;; around this.
(define (hide-provide stx)
  (let go ([e stx]
           [inside? #f])
    (cond
      [(syntax? e) (datum->syntax e (go (syntax-e e) inside?) e e)]
      [(and (pair? e))
       (cond
         [(and (eq? (syntax-e (car e)) 'module) inside?) e]
         [(eq? (syntax-e (car e)) 'provide)
          (cons 'scv-cr:provide (cdr e))]
         [else (cons (go (car e) #t) (go (cdr e) #t))])]
      [else e])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inject

;; Contracts Syntax Boolean → Syntax
;; Returns new require syntax to be injected into a module according to the
;; bundle.
(define (require-inject ctcs lang)
  (define bundle (contracts-require ctcs))
  (with-syntax ([?prov (provide-inject bundle)]
                [(?lib ...) (contracts-libs ctcs)])
    #`(begin
        (module require/safe racket/base
          (require racket/contract ?lib ...)
          ?prov)
        (require 'require/safe))))

;; Bundle Boolean → Syntax
;; Returns new provide syntax to be injected into a module according to the
;; bundle. We include the require here so that identifiers from spliced in
;; syntax can be loaded. We only want to instantiate the module not import
;; anything which is why we use only-in.
(define (provide-inject bundle)
  (define defn-hash (bundle-definitions bundle))
  (with-syntax ([(exp ...)  (provide-exports bundle #t)]
                [(ctc ...)  (hash-keys defn-hash)]
                [(dep ...)  (bundle-deps bundle)]
                [(defn ...) (provide-defns defn-hash)])
    #`(begin (provide exp ... ctc ...)
             (require (only-in (combine-in dep ...)))
             defn ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide inject helpers

;; [Hash Symbol Syntax] → [Listof Syntax]
;; Returns the list of contract definitions.
(define (provide-defns defn-hash)
  (for/list ([name (in-list (sort (hash-keys defn-hash) </id))])
    (define defn (hash-ref defn-hash name))
    #`(define #,name #,defn)))

;; Bundle Boolean → [Listof Syntax]
;; Returns the list of contract-out items.
(define (provide-exports bundle safe?)
  (append (single-outs bundle safe?)
          (struct-outs bundle safe?)))

;; Bundle Boolean → Syntax
;; Returns contract-out syntax for all individual exports.
(define (single-outs bundle safe?)
  (define omit (structs-exports (bundle-structs bundle)))
  (for/list ([(name ctc) (in-hash (bundle-exports bundle))]
             #:unless (member name omit))
    (if (and ctc safe?)
        #`(contract-out
           #,(syntax-property #`[#,name #,ctc] 'parent-identifier name))
        (datum->syntax #f name))))

;; Bundle Boolean → Syntax
;; Returns contract-out syntax for structs.
(define (struct-outs bundle safe?)
  (for/list ([(name data) (in-hash (bundle-structs bundle))])
    (match-define (struct-data parent fields contracts) data)
    (define id+parent (if parent (list name parent) name))
    (define field+ctcs (map list fields contracts))
    (if safe?
        #`(contract-out
           #,(syntax-property #`(struct #,id+parent #,field+ctcs)
                              'parent-identifier
                              name))
        #`(struct-out #,name))))

;; Bundle → [Listof Syntax]
;; Determines the list of modules that are needed for the definitions of the
;; bundle.
(define (bundle-deps bundle)
  (define dependencies
    (list->set
     (for/append ([v (in-hash-values (bundle-definitions bundle))])
       (syntax-deps v))))
  (set-map dependencies (λ (x) (datum->syntax #f x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           racket/function
           racket/path
           racket/port
           racket/unsafe/ops
           rackunit
           soft-contract/main
           "extract.rkt"
           "../test/path.rkt"
           "../test/expand.rkt")

  (define (uncontract stx)
    (syntax-case stx (contract-out)
      [(contract-out x) (syntax->datum #'x)]
      [_ (raise-result-error 'uncontract "did not get a contract-out form")]))

  (define streams-mod (make-mod streams))
  (define streams-ctcs (make-contracts streams-expand))
  (define streams-provide (contracts-provide streams-ctcs))
  (define sieve-main-mod (make-mod sieve-main))

  (dynamic-wind
   cleanup-bytecode
   (λ ()
     (test-case "elaborate (sieve)"
       (define 0-10
         (parameterize ([current-namespace (make-base-namespace)])
           (eval (mod-syntax streams-mod))
           (namespace-require ''streams)
           (eval #'(letrec ([f (λ (n)
                                 (λ ()
                                   (make-stream n (f (add1 n)))))])
                     (stream-take ((f 0)) 10)))))
       (compile-modules (list streams-mod sieve-main-mod))
       (define sieve-output
         (with-output-to-string
           (λ ()
             (dynamic-require (string->path sieve-main) #f))))
       (chk
        0-10 '(0 1 2 3 4 5 6 7 8 9)
        #:t (regexp-match? #rx"cpu time:" sieve-output)
        ))

     (test-case "elaborate (client and server)"
       (define (chk-elaborate server-path client-path)
         (define server (make-mod server-path))
         (define client (make-mod client-path))
         (compile-modules (filter mod-typed? (list server client)))
         (chk
          (verify-modules (list server-path client-path)
                          (list (mod-syntax server) (mod-syntax client)))
          '())
         (parameterize ([current-namespace (make-base-namespace)]
                        [current-directory (path-only server-path)])
           (eval (mod-syntax client))
           (namespace-require ''client)
           (chk (unsafe-struct-ref (eval #'(g 42)) 0) 42)))
       (chk-elaborate ty-ty-server ty-ty-client)
       (chk-elaborate ty-ut-server ty-ut-client)
       (chk-elaborate ut-ty-server ut-ty-client)
       (chk-elaborate ut-ut-server ut-ut-client))

     (test-case "single-outs"
       (chk
        #:eq set=?
        (map (compose first uncontract) (single-outs streams-provide #t))
        '(stream-take stream-unfold stream-get make-stream)))

     (test-case "struct-outs"
       (chk
        #:t
        (match (map uncontract (struct-outs streams-provide #t))
          [`((struct stream ((first ,_) (rest ,_)))) #t]
          [_ #f])))

     (test-case "bundle-deps"
       (define deps
         '((lib "racket/contract/base.rkt")
           (lib "racket/base.rkt")
           (lib "racket/contract.rkt")))

       (chk
        #:eq set=?
        deps
        (map syntax->datum (bundle-deps streams-provide))
        )))
   cleanup-bytecode)
  )
