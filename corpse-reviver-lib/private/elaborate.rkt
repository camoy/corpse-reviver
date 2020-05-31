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
         "logging.rkt"
         "struct.rkt"
         "syntax.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mod

;; Path-String → Mod
;; Returns a module from a file and syntax.
(define (make-mod target)
  (debug "elaborating: ~a" target)
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
  (debug "elaborated (~a):\n~a." target elaborated)
  (mod target
       raw-stx elaborated ctcs #t (imports target)
       (contract-positions elaborated)
       (contract-dependency elaborated)))

;; Path-String Syntax → Module
;; Returns module for an untyped file from raw syntax. This does nothing since
;; untyped code needs no elaboration.
(define (untyped->mod target raw-stx)
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
       #:with ?prov (provide-inject prov-bundle #f)
       #:with ?req  (require-inject ctcs #'?lang/nc)
       #:with ?bodies (mangle-provides #'(begin ?body ...))
       #`(module ?name ?lang/nc
           (module #%type-decl racket/base)
           (register-contracts! ?sneak)
           ?req ?bodies ?prov)]))
  (~> stx
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
;; Hide provide forms and move them to the bottom of the module. This is
;; necessary because SCV directly looks at forms labeled as provide. We
;; need a level of indirection (scv-cr:provide) to get around this.
;; Moving all provides to the end is needed to avoid problems when exporting
;; syntax (like types) before they're defined.
(define (mangle-provides stx)
  (define provides '())
  (define stx*
    (let go ([e stx])
      (cond
        [(syntax? e) (datum->syntax e (go (syntax-e e)) e e)]
        [(and (pair? e))
         (cond
           [(and (eq? (syntax-e (car e)) 'module)) e]
           [(eq? (syntax-e (car e)) 'provide)
            (set! provides (cons #`(scv-cr:provide #,@(cdr e)) provides))
            '(void)]
           [else (cons (go (car e)) (go (cdr e)))])]
        [else e])))
  #`(begin #,stx* #,@provides))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inject

;; Contracts Syntax Boolean → Syntax
;; Returns new require syntax to be injected into a module according to the
;; bundle.
(define (require-inject ctcs lang)
  (define bundle (contracts-require ctcs))
  (with-syntax ([?prov (provide-inject bundle #t)]
                [(?opaque ...) (contracts-opaques ctcs)]
                [(?lib ...) (contracts-libs ctcs)])
    #`(begin
        (module require/safe typed/racket/base/no-check
          (require soft-contract/fake-contract ?lib ...)
          ?opaque ... ?prov)
        (require/define 'require/safe
                        #,(set-subtract (hash-keys (bundle-exports bundle))
                                        (structs-exports (bundle-structs bundle)))
                        #,(hash-keys (bundle-structs bundle))))))

;; Bundle Boolean → Syntax
;; Returns new provide syntax to be injected into a module according to the
;; bundle. We include the require here so that identifiers from spliced in
;; syntax can be loaded. We only want to instantiate the module not import
;; anything which is why we use only-in.
(define (provide-inject bundle with-contracts?)
  (define defn-hash (bundle-definitions bundle))
  (with-syntax ([(exp ...)  (provide-exports bundle #t)]
                [(ctc ...)  (if with-contracts?
                                (hash-keys defn-hash)
                                #'())]
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

  (around
   cleanup-bytecode

   (with-chk (['name "elaborate (sieve)"])
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

   (with-chk (['name "elaborate (client and server)"])
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

   (with-chk (['name "mangle-provides"])
     (chk
      (syntax->datum
       (mangle-provides #'(begin (add1 42) (provide x) (add1 3) (provide y))))
      '(begin (begin (add1 42) (void) (add1 3) (void))
              (scv-cr:provide y)
              (scv-cr:provide x))))

   (with-chk (['name "single-outs"])
     (chk
      #:eq set=?
      (map (compose first uncontract) (single-outs streams-provide #t))
      '(stream-take stream-unfold stream-get make-stream)))

   (with-chk (['name "struct-outs"])
     (chk
      #:t
      (match (map uncontract (struct-outs streams-provide #t))
        [`((struct stream ((first ,_) (rest ,_)))) #t]
        [_ #f])))

   (with-chk (['name "bundle-deps"])
     (define deps
       '((lib "racket/contract/base.rkt")
         (lib "racket/base.rkt")
         (lib "racket/contract.rkt")))

     (chk
      #:eq set=?
      deps
      (map syntax->datum (bundle-deps streams-provide))
      ))

   cleanup-bytecode)
  )
