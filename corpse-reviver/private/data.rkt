#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  (struct mod
    ([target (and/c string? complete-path?)]
     [raw syntax?]
     [syntax syntax?]
     [contracts (or/c contracts? #f)]
     [typed? boolean?]
     [imports (listof symbol?)]
     [positions (or/c (hash/c (cons/c integer? integer?) symbol?) #f)]
     [deps (or/c unweighted-graph? #f)]))

  (struct contracts
    ([provide bundle?]
     [require bundle?]
     [predicates (hash/c any/c syntax?)]))

  (struct bundle
    ([definitions definitions/c]
     [exports exports/c]
     [structs structs/c]
     [libs libs/c]))

  (struct struct-data
    ([parent (or/c symbol? #f)]
     [fields (listof symbol?)]
     [contracts (listof syntax?)]))

  [current-write-contracts? (parameter/c boolean?)]
  [current-typed-blame? (parameter/c boolean?)]
  [definitions/c contract?]
  [exports/c contract?]
  [structs/c contract?])

 contract-sc
 libs/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data definitions

;; An Mod is a struct where
;;   target is a path to the module,
;;   raw is the syntax of the original module,
;;   syntax is the syntax of the elaborated module,
;;   contracts contains all information needed for elaboration,
;;   typed? indicates if the module was originally typed,
;;   imports is a list of dependencies,
;;   positions maps a line-column pair to the binding it's contained in,
;;   deps is a graph of contract dependencies.
(struct
 mod
 (target raw syntax contracts typed? imports positions deps)
 #:prefab)

;; Contracts is a struct where
;;   provide contains information for contracts on exports,
;;   require contains information for contracts on untyped imports,
;;   predicates maps predicates, as an s-expression, to their definition
;;     (these predicates come from make-predicate or define-predicate).
(struct contracts (provide require predicates) #:prefab)

;; A Bundle is a struct where
;;   definitions maps an identifier representing a contract to its definition,
;;   exports maps an export to its contract or #f to be uncontracted,
;;   structs maps struct names to their information,
;;   libs is a list of libraries imported with require/typed.
(struct bundle (definitions exports structs libs) #:prefab)

;; A Struct-Data is a struct where
;;   parent is the parent struct name or #f if there is none,
;;   fields is a list of fields,
;;   contracts is a list of field contracts.
(struct struct-data (parent fields contracts) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameters

;; [Parameter Boolean]
;; Whether to write the contracted version of the files.
(define current-write-contracts? (make-parameter #f))

;; [Parameter Boolean]
;; Whether to keep blame reported for typed modules.
(define current-typed-blame? (make-parameter #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; etc

;; The scope for introduced contracts.
(define contract-sc (make-syntax-introducer))

;; Contract → Contract
;; Constructs a contract for immutable hashes from symbols to the contract.
(define (hash-symbol/c ctc)
  (hash/c symbol? ctc #:immutable #t))

;; Contracts
(define definitions/c (hash-symbol/c syntax?))
(define exports/c (hash-symbol/c (or/c #f syntax?)))
(define structs/c (hash-symbol/c struct-data?))
(define libs/c (hash-symbol/c syntax?))
