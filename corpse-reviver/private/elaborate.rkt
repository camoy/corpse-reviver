#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [make-mod (-> path-string? mod?)]
  [elaborate (-> mod? boolean? mod?)]))

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
         "prepare.rkt"
         "struct.rkt"
         "syntax.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mod

;; Path-String → Mod
;; Returns a module from a file and syntax.
(define (make-mod target)
  (define raw-stx (syntax-fetch target))
  (if (typed? target)
      (typed->mod target raw-stx)
      (untyped->mod target raw-stx)))

;; Path-String Syntax → Mod
;; Returns module for a typed file from raw syntax.
(define (typed->mod target raw-stx)
  (define expanded-stx (expand/dir target raw-stx))
  (define contracts (make-contracts expanded-stx))
  (compile+write/dir target expanded-stx)
  (mod target raw-stx raw-stx contracts #t (imports target) #f #f))

;; Path-String Syntax → Module
;; Returns module for an untyped file from raw syntax. This does nothing since
;; untyped code needs no elaboration.
(define (untyped->mod target raw-stx)
  (compile+write/dir target raw-stx)
  (mod target raw-stx raw-stx #f #f (imports target) #f #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elaborate

;; [Parameter Boolean]
;; Parameter for if the elaboration should be made for input to SCV.
(define current-scv? (make-parameter #f))

;; Mod Boolean → Syntax
;; Elaborates a module according to its contracts.
(define (elaborate m scv?)
  (define elaborated-stx
    (parameterize ([current-scv? scv?])
      (-elaborate m)))
  (struct-copy mod m
               [syntax elaborated-stx]
               [positions (contract-positions elaborated-stx)]
               [deps (contract-dependency elaborated-stx)]))

;; Mod → Syntax
;; Elaborates syntax according to their contracts.
(define (-elaborate m)
  (define ctcs (mod-contracts m))
  (define prov-bundle (contracts-provide ctcs))
  (define stx
    (syntax-parse (mod-syntax m)
      #:datum-literals (module)
      [(module ?name ?lang (?mb ?body ...))
       #:with ?lang/nc (no-check #'?lang)
       #:with ?prov (provide-inject prov-bundle)
       #:with ?req  (require-inject ctcs #'?lang/nc)
       (prepare ctcs
                #`(module ?name ?lang/nc
                    #,prelude ?prov ?req ?body ...))]))
  (~> stx
      contract-sc
      (normalize-srcloc (mod-target m))))

;; Syntax Boolean → Syntax
;; Returns the name of the no-check language for the new module.
(define (no-check lang)
  (match (syntax-e lang)
    ['typed/racket/base
     (if (current-scv?)
         #'corpse-reviver/private/lang/scv/base
         #'corpse-reviver/private/lang/normal/base)]
    ['typed/racket
     (if (current-scv?)
         #'corpse-reviver/private/lang/scv/full
         #'corpse-reviver/private/lang/normal/full)]
    [else (error 'no-check "unknown language ~a" lang)]))

;; Datum
;; This is prefixed onto all elaborated modules. It fakes being a typed module
;; and defines a flag indicating that this is an SCV-CR module. Both are needed
;; for require/typed/check to work correctly.
(define prelude
  #'(begin (module #%type-decl racket/base)
           (require (for-syntax racket/base))
           (define-syntax scv-cr? #t)))

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
        (module require/safe #,lang
          #,prelude
          (require ?lib ...)
          ?prov)
        (require/define 'require/safe
                        #,(hash-keys (bundle-exports bundle))
                        #,(structs-exports (bundle-structs bundle))))))

;; Bundle Boolean → Syntax
;; Returns new provide syntax to be injected into a module according to the
;; bundle. We include the require here so that identifiers from spliced in
;; syntax can be loaded. We only want to instantiate the module not import
;; anything which is why we use only-in.
(define (provide-inject bundle)
  (define defn-hash (bundle-definitions bundle))
  (with-syntax ([(exp ...)  (provide-exports bundle #t)]
                [(exp-unsafe ...)  (provide-exports bundle #f)]
                [(ctc ...)  (hash-keys defn-hash)]
                [(dep ...)  (bundle-deps bundle)]
                [(defn ...) (provide-defns defn-hash)])
    (define unsafe-submodule
      (if (current-scv?)
          #'(void)
          #'(module+ provide/unsafe
              (scv:provide exp-unsafe ...))))
    #`(begin (scv:provide exp ...)
             #,unsafe-submodule
             (scv:ignore (provide ctc ...))
             (scv:ignore (require (only-in (combine-in dep ...))))
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
           rackunit
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

  (test-case "elaborate"
    (define streams-mod* (elaborate streams-mod #t))
    (define 0-10
      (parameterize ([current-namespace (make-base-namespace)])
        (eval (mod-syntax streams-mod*))
        (namespace-require ''streams)
        (eval #'(letrec ([f (λ (n)
                              (λ ()
                                (make-stream n (f (add1 n)))))])
                  (stream-take ((f 0)) 10)))))
    (chk
     0-10 '(0 1 2 3 4 5 6 7 8 9)
     ))

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
     ))
  )
