#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/contract)
(provide
 (contract-out
  [contract-positions (-> syntax? (hash/c (cons/c integer? integer?) symbol?))]
  [contract-dependency (-> syntax? unweighted-graph?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require graph
         racket/function
         racket/hash
         racket/list
         syntax/parse
         threading)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntax → [Hash [Cons Integer Integer] Symbol]
;; Returns a hash that maps line-column pairs to the contract definition that
;; position occurs in.
(define (contract-positions stx)
  (make-immutable-hash
    (let go ([parent #f]
             [stx stx])
      (syntax-parse stx
        [(x ...)
         (define parent* (or (syntax-property stx 'parent-identifier) parent))
         (define rest (append-map (λ~>> (go parent*)) (attribute x)))
         (if parent*
             `(((,(syntax-line stx) . ,(syntax-column stx)) . ,parent*) . ,rest)
             rest)]
        [x:id
         (if parent
             `(((,(syntax-line stx) . ,(syntax-column stx)) . ,parent))
             null)]
        [_ null]))))

;; Syntax → Unweighted-Graph
;; Returns a graph where an edge goes from an identifier to its parent contract.
(define (contract-dependency stx)
  (unweighted-graph/directed
    (let go ([parent #f]
             [stx stx])
      (syntax-parse stx
        [(x ...)
         (define parent* (or (syntax-property stx 'parent-identifier) parent))
         (append-map (go parent* _) (attribute x))]
        [x:id (if parent (list (list (syntax-e #'x) parent)) null)]
        [_ null]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require chk)

  (define correct-line (add1 (syntax-line #'_)))
  (define stx (syntax-property #'(-> g0 g1)
                               'parent-identifier
                               'generated-contract0))

  (chk
   (contract-positions stx)
   (hash (cons correct-line 40) 'generated-contract0
         (cons correct-line 37) 'generated-contract0
         (cons correct-line 33) 'generated-contract0
         (cons correct-line 34) 'generated-contract0)

   (get-edges (contract-dependency stx))
   '((g0 generated-contract0) (g1 generated-contract0) (-> generated-contract0))
   ))
