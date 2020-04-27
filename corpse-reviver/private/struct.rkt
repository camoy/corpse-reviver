#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [structs (-> symbol? syntax? definitions/c exports/c structs/c)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require fancy-app
         mischief/for
         racket/list
         racket/match
         racket/syntax
         "data.rkt"
         "syntax.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; Symbol Syntax Definitions Exports → Structs
;; Construct a structure containing information about provided or required
;; structs.
(define (structs key stx defns exports)
  (define props (syntax-property-values stx (format-symbol "~a-struct" key)))
  (define-values (local-fields names)
    (struct-local-fields props defns exports))
  (struct-all-fields local-fields names))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; util

;; [Listof Vector] Definitions Exports → Structs [Listof Symbol]
;; Construct a structs hash, only with local fields and contracts (not anything
;; from the super). Also return a list of struct names sorted by ancestry.
(define (struct-local-fields props defns exports)
  (define/for/fold ([result (hash)]
                    [parent-tree (hash)])
                   ([prop (in-list props)])
    (match-define (vector name-stx parent-stx pred-stx sels muts) prop)
    (define name (syntax-e name-stx))
    (define parent (and (identifier? parent-stx) (syntax-e parent-stx)))
    (define/for/lists (fields contracts)
                      ([sel (in-list sels)]
                       #:when (field-name name sel))
      (define ctc (hash-ref exports (syntax-e sel)))
      (values (field-name name sel)
              (chase-codomain defns ctc)))
    (values (hash-set result name (struct-data parent fields contracts))
            (hash-set parent-tree name (list parent))))
  (values result (topological-sort parent-tree)))

;; Structs [Listof Symbol] → Structs
;; Given a structs hash with local fields and a sorted list of struct names, fill
;; in the rest of the fields by appending the parent's fields and contracts.
(define (struct-all-fields local-fields names)
  (for/fold ([result local-fields])
            ([name (in-list names)])
    (define data (hash-ref result name))
    (define parent (struct-data-parent data))
    (define-values (parent-fields parent-contracts)
      (if parent
          (let ([parent-data (hash-ref result parent)])
            (values (struct-data-fields parent-data)
                    (struct-data-contracts parent-data)))
          (values null null)))
    (define data*
      (struct-copy
       struct-data data
       [fields (append parent-fields (struct-data-fields data))]
       [contracts (append parent-contracts (struct-data-contracts data))]))
    (hash-set result name data*)))

;; Symbol Syntax → Symbol
;; Returns the field name associated with a selector.
(define (field-name name selector)
  (define pat
    (pregexp (format "^~a-(.+)$" (regexp-quote (symbol->string name)))))
  (define result
    (regexp-match pat (symbol->string (syntax-e selector))))
  (match result
    [(list _ fld) (string->symbol fld)]
    [_ #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           rackunit
           "../test/expand.rkt")

  (define-values (i? r?)
    (values #'integer? #'real?))

  (define defns
    (hash 'a #`(-> child? #,i?)
          'b #`(-> parent? #,r?)))

  (define exports
    (hash 'child-c #'a
          'parent-p #'b))

  (define expected-structs
    (hash 'child (struct-data 'parent '(p c) (list r? i?))
          'parent (struct-data #f '(p) (list r?))))

  (define-values (local-fields names)
    (struct-local-fields
     (syntax-property-values ty-ty-server-expand 'provide-struct)
     defns exports))

  (test-case "structs"
    (chk
     (structs 'provide ty-ty-server-expand defns exports)
     expected-structs

     (structs 'require ty-ut-client-expand defns exports)
     expected-structs))

  (test-case "struct-local-fields"
    (chk
     local-fields
     (hash 'child
           (struct-data 'parent '(c) (list i?))
           'parent
           (struct-data #f '(p) (list r?)))
     names
     '(parent child)))

  (test-case "struct-all-fields"
    (chk
     (struct-all-fields local-fields names)
     expected-structs))

  (test-case "field-name"
    (chk
     (field-name 'stream #'stream-first) 'first
     #:! #:t (field-name 'stream #'foo-first)))
  )
