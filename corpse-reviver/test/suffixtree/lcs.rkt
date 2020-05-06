#lang typed/racket/base
;; Some utilities.

(require corpse-reviver/require-typed-check
         (except-in "typed-data.rkt" label)
         racket/list)

(require/typed/check "label.rkt"
 [label->string (-> Label String)]
 [string->label (-> String Label)]
 [string->label/with-sentinel (-> String Label)]
 [label (-> (U String (Vectorof (U Char Symbol))) Label)]
 [label-source-eq? (-> Label Label Boolean)]
 [label-length (-> Label Index)]
 [vector->label (-> (Vectorof (U Char Symbol)) Label)]
 [label-ref (-> Label Integer (U Symbol Char))])

(require/typed/check
 "structs.rkt"
 [make-tree (-> Tree)]
 [tree-root (-> Tree Node)]
 )

(require/typed/check "ukkonen.rkt"
 [tree-add! (-> Tree Label Void)])

(: false-thunk (-> #f))
(define false-thunk (lambda () #f))


;; longest-common-substring: string string -> string
;; Returns the longest common substring between the two strings.
(provide longest-common-substring)
(: longest-common-substring (-> String String String))
(define (longest-common-substring s1 s2)
  (label->string (longest-common-sublabel (string->label/with-sentinel s1)
                                          (string->label/with-sentinel s2))))

;; longest-common-sublabel: label label -> label
;;
;; Naive use of suffix trees to find longest common sublabel between
;; two labels.  Note that there's a better way to do this with
;; matching statistics: I'll try using matching statistics as soon
;; as I get this version running.
;;
;; This approach simply adds both labels to a common suffix tree,
;; does a postorder traversal to mark up the inner nodes, and then
;; finds the inner node with the deepest string depth.
(provide longest-common-sublabel)
(: longest-common-sublabel (-> Label Label Label))
(define (longest-common-sublabel label-1 label-2)
  (: label-1-marks (HashTable Node Boolean))
  (define label-1-marks (make-hasheq))
  (: label-2-marks (HashTable Node Boolean))
  (define label-2-marks (make-hasheq))
  (: deepest-node Node)
  (define deepest-node (node (label "no lcs") #f '() #f))
  (: deepest-depth Index)
  (define deepest-depth 0)
  (: main (-> Label))
  (define (main)
    (define tree (make-tree))
    (tree-add! tree label-1)
    (tree-add! tree label-2)
    (mark-up-inner-nodes! (tree-root tree) 0)
    (path-label deepest-node))
  (: mark-up-inner-nodes! (-> Node Index Void))
  (define (mark-up-inner-nodes! node depth)
    (cond [(null? (node-children node))
           (when (label-source-eq? (node-up-label node) label-1)
             (mark-with-label-1! node))
           (when (label-source-eq? (node-up-label node) label-2)
             (mark-with-label-2! node))]
          [else
            (for ([child (node-children node)])
              (let ([i (+ depth (label-length (node-up-label child)))])
                (unless (index? i) (error "NOOOOO"))
                (mark-up-inner-nodes! child i)))
            (absorb-children-marks! node depth)]))
  (: mark-with-label-1! (-> Node Void))
  (define (mark-with-label-1! node)
    (hash-set! label-1-marks node #t))
  (: mark-with-label-2! (-> Node Void))
  (define (mark-with-label-2! node)
    (hash-set! label-2-marks node #t))
  (: marked-by-label-1? (-> Node Boolean))
  (define (marked-by-label-1? node)
    (hash-ref label-1-marks node false-thunk))
  (: marked-by-label-2? (-> Node Boolean))
  (define (marked-by-label-2? node)
    (hash-ref label-2-marks node false-thunk))
  (: marked-by-both? (-> Node Boolean))
  (define (marked-by-both? node)
    (and (marked-by-label-1? node)
         (marked-by-label-2? node)))
  (: absorb-children-marks! (-> Node Index Void))
  (define (absorb-children-marks! node depth)
    ;(let/ec escape
      (for ([child (node-children node)])
        (when (marked-by-label-1? child)
          (mark-with-label-1! node))
        (when (marked-by-label-2? child)
          (mark-with-label-2! node)))
        ;(when (marked-by-both? node)
        ;  (escape))))
    (when (and (marked-by-both? node)
               (> depth deepest-depth))
      (set! deepest-depth depth)
      (set! deepest-node node)))
  (if (or (= 0 (label-length label-1))
          (= 0 (label-length label-2)))
      (string->label "")
      (main)))



;; path-label: node -> label
;;
;; Returns a new label that represents the path from the tree root
;; to this node.
;;
;; Fixme: optimize the representation of label to be able to do this
;; without much reallocation.  Maybe another label class that uses a
;; rope data structure might be better...  I need to read Hans
;; Boehm's paper on "Ropes, an alternative to strings" to see how
;; much work this would be.
(provide path-label)
(: path-label (-> Node Label))
(define (path-label node)
  (: collect-loop (-> (U Node #f) (Listof Label) Integer Label))
  (define (collect-loop current-node collected-labels total-length)
    (if current-node
      (collect-loop (node-parent current-node)
                    (cons (node-up-label current-node) collected-labels)
                    (+ total-length
                       (label-length (node-up-label current-node))))
      (build-new-label collected-labels total-length)))
  (: vector-blit! (-> Label (Vectorof (U Char Symbol)) Index Void))
  (define (vector-blit! src-label dest-vector dest-offset)
    (let loop ((i 0))
      (let ([index (+ i dest-offset)])
      (when (and (< i (label-length src-label)) (index? i) (index? index))
        (vector-set! dest-vector
                     index
                     (label-ref src-label i))
        (loop (add1 i))))))
  (: build-new-label (-> (Listof Label) Integer Label))
  (define (build-new-label labels total-length)
    (: vector (Vectorof (U Char Symbol)))
    (define vector (make-vector total-length 'X))
    (let loop ((labels labels) (i 0))
      (cond [(null? labels)
             (vector->label vector)]
            [(index? i)
              (vector-blit! (car labels) vector i)
              (loop (cdr labels)
                    (+ i (label-length (car labels))))]
            [else (error "not an index")])))
  (collect-loop node '() 0))

