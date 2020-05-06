#lang typed/racket/base

;; Much of this comes from reading Dan Gusfield's Algorithms on
;; strings, trees, and sequences: computer science and computational
;; biology.

(require
 (except-in "typed-data.rkt" label)
 corpse-reviver/require-typed-check
)
(require/typed/check "label.rkt"
  [label-length (-> Label Index)]
  [label-ref (-> Label Integer (U Symbol Char))]
  [label->string (-> Label String)]
  [string->label (-> String Label)]
  [string->label/with-sentinel (-> String Label)]
  [label-element-equal? (-> Any Any Boolean)]
  [label-source-eq? (-> Label Label Boolean)]
  [vector->label (-> (Vectorof (U Char Symbol)) Label)]
  [label (-> (U String (Vectorof (U Char Symbol))) Label)]
  [sublabel (case-> (-> Label Index Label)
                    (-> Label Index Index Label))])

(require/typed/check "structs.rkt"
  [new-suffix-tree (-> Tree)]
  [node-find-child (-> Node Any (U Node #f))]
  [node-root? (-> Node Boolean)]
  [node-position-at-end? (-> Node Index Boolean)]
  [node-add-leaf! (-> Node Label Node)]
  [node-up-splice-leaf! (-> Node Index Label (values Node Node))]
  [node-follow/k (-> Node
                     Label
                     (-> Node (Pairof Node Index))
                     (-> Node Index (Pairof Node Index))
                     (-> Node Label Index (Pairof Node Index))
                     (-> Node Index Label Index (Pairof Node Index))
                     (Pairof Node Index))]
  )

(define dummy-node (node (label "dummy") #f '() #f))


;; skip-count: node label -> (values node number)
;;
;; Follows down the node using the skip-count rule until we exhaust
;; the label.  Assumes that there does exist a labeled path starting
;; from the node that exactly matches label.
(provide skip-count)
(: skip-count (-> Node Label (values Node Index)))
(define (skip-count node label)
  (define l-l (label-length label))
  (unless (index? l-l) (error "skip-count"))
  (skip-count-helper node label 0 l-l))

;; Utility function for skip count, but also visible for those in
;; the know to skip-count from an arbitrary position in label.
(: skip-count-helper (-> Node Label Index Index (values Node Index)))
(define (skip-count-helper node label k N)
  (: loop (-> Node Integer (values Node Index)))
  (define (loop node k)
    (let* ((child (node-find-child node (label-ref label k)))
           (child-label (begin
                          (unless child (error "skip-count-hlper"))
                          (node-up-label child)))
           (child-label-length (label-length child-label))
           (rest-of-chars-left-to-skip (- N k)))
      (if (> rest-of-chars-left-to-skip child-label-length)
          (loop child
                (+ k child-label-length))
          (begin (unless (index? rest-of-chars-left-to-skip) (error "skip-count=hlper !!!"))
                 (values child rest-of-chars-left-to-skip)))))
  (if (>= k N)
      (values node (label-length (node-up-label node)))
      (loop node k)))


;; jump-to-suffix: node -> (values node (union boolean number))
;;
;; Given an internal node, jumps to the suffix from that node.
;; According to the theory of suffix trees, such a node will exist
;; in the tree if we follow the Ukkonen construction.  If we had to
;; go up a few characters, returns the number of chars at the suffix
;; end that need to be compared to get the real suffix.

;; If we hit the root, that offset is #f to indicate that we have to
;; start searching the suffix from scratch.
(provide jump-to-suffix)
(: jump-to-suffix (-> Node (values Node (U Boolean Integer))))
(define (jump-to-suffix node)
  (define PARENT (node-parent node))
  (cond ((node-root? node)
         (values node #f))
        ((node-suffix-link node)
         (begin 
                (let ([node2 (node-suffix-link node)])
                  (unless node2 (error "jump to suffix"))
                  (values node2 0))))
        ((and PARENT (node-root? PARENT))
           (values PARENT #f))
        (else
         (let* ([parent (node-parent node)]
                [sl (begin (unless parent (error "j2s"))
                           (node-suffix-link parent))])
           (unless sl (error "j2s whoahao"))
           (values sl
                   (label-length (node-up-label node)))))))

(provide try-to-set-suffix-edge!)
;; try-to-set-suffix-edge!: node node -> void
;;
;; Sets the suffix edge of from-node directed to to-node if it
;; hasn't been set yet.
(: try-to-set-suffix-edge! (-> Node Node Void))
(define (try-to-set-suffix-edge! from-node to-node)
  (when (not (node-suffix-link from-node))
    (set-node-suffix-link! from-node to-node)))

;; find-next-extension-point/add-suffix-link!: node label number number ->
;;     (values node number number)
;;
;; Given the last active node where an extension was last made,
;; looks for the next position for extension.  Returns that
;; extension point's node and label offset, as well as the new phase
;; number i.  (Postcondition: (>= i initial-i))
;;
;; The first pass through the loop is a special case: we set the
;; suffix link from node to suffix-node unless we expect it to be
;; done from a splicing extension.
;;
;; If we run off the label (implicit tree), returns (values #f #f #f).
(provide find-next-extension-point/add-suffix-link!)
(: find-next-extension-point/add-suffix-link! (-> Node Label Index Index (values (U #f Node) (U #f Index) (U #f Index))))
(define (find-next-extension-point/add-suffix-link! node label initial-i j)
  (: fixed-start (-> (U Integer #f) Index))
  (define (fixed-start suffix-offset)
    (let ([i (if suffix-offset (- initial-i suffix-offset) j)])
      (unless (index? i) (error "find-next")) i))
  (define-values (suffix-node suffix-offset)
    (jump-to-suffix node))
  (define K
    (fixed-start (cond [(integer? suffix-offset) suffix-offset]
                       [(eq? #t suffix-offset) 1]
                       [(eq? #f suffix-offset) #f]
                       [else (error "find-next")])))
  (define N
    (let ([i (label-length label)])
      (unless (index? i) (error "find-next")) i))
   (: loop-first (-> Index (values (U #f Node) (U #f Index) (U #f Index))))
   (define (loop-first i)
     (loop-general i (lambda ([skipped-node : Node]
                              [skip-offset  : Index])
                       (when (node-position-at-end? skipped-node skip-offset)
                         (try-to-set-suffix-edge! node skipped-node)))))
   (: loop-rest (-> Index (values (U #f Node) (U #f Index) (U #f Index))))
   (define (loop-rest i)
     (loop-general i (lambda ([skipped-node : Node] [skip-offset : Index])
                       (void))))
   (: loop-general (-> Index (-> Node Index Void) (values (U #f Node) (U #f Index) (U #f Index))))
   (define (loop-general i first-shot)
     (cond [(>= i N) (values #f #f #f)]
           [else 
            (define-values (skipped-node skipped-offset)
              (skip-count-helper suffix-node label K i))
            (first-shot skipped-node skipped-offset)
             (if (node-position-at-end? skipped-node skipped-offset)
                 (find-extension-at-end! skipped-node skipped-offset i)
                 (find-extension-in-edge skipped-node skipped-offset i))]))
   (: find-extension-in-edge (-> Node Index Index (values (U #f Node) (U #f Index) (U #f Index))))
   (define (find-extension-in-edge skipped-node skip-offset i)
     (cond [(label-element-equal?
             (label-ref label i)
             (label-ref (node-up-label skipped-node) skip-offset))
            (let ([n (add1 i)]) (unless (index? n) (error "find-next")) (loop-rest n))]
           [else (values skipped-node skip-offset i)]))
   (: find-extension-at-end! (-> Node Index Index (values (U #f Node) (U #f Index) (U #f Index))))
   (define (find-extension-at-end! skipped-node skip-offset i)
     (cond [(node-find-child skipped-node (label-ref label i))
            (let ([n (add1 i)]) (unless (index? n) (error "find-next")) (loop-rest n))]
           [else (values skipped-node skip-offset i)]))
   (loop-first initial-i))


(provide extend-at-point!)
;; extend-at-point!: node number label number -> node
(: extend-at-point! (-> Node Index Label Index Node))
(define (extend-at-point! node offset label i)
  (: main-logic (-> Node Index Label Index Node))
  (define (main-logic node offset label i)
    (if (should-extend-as-leaf? node offset)
        (attach-as-leaf! node label i)
        (splice-with-internal-node! node offset label i)))
  (: should-extend-as-leaf? (-> Node Index Boolean))
  (define (should-extend-as-leaf? node offset)
    (node-position-at-end? node offset))
  (: attach-as-leaf! (-> Node Label Index Node))
  (define (attach-as-leaf! node label i)
    (: leaf Node)
    (define leaf (node-add-leaf! node (sublabel label i)))
    node)
  (: splice-with-internal-node! (-> Node Index Label Index Node))
  (define (splice-with-internal-node! node offset label i)
    ;; otherwise, extend by splicing
    (define-values (split-node leaf)
      (node-up-splice-leaf!
       node offset (sublabel label i)))
    split-node)
  (main-logic node offset label i))

(provide suffix-tree-add!)
;; suffix-tree-add!: tree label -> void
;; Adds a new label and its suffixes to the suffix tree.
;; Precondition: label is nonempty.
(: suffix-tree-add! (-> Tree Label Void))
(define (suffix-tree-add! tree label)
  (: do-construction! (-> Tree Label Void))
  (define (do-construction! tree label)
    (define pr (add-first-suffix! tree label))
    (define starting-node (car pr))
    (define starting-offset (cdr pr))
    (add-rest-suffixes! label starting-node starting-offset))
  (: add-first-suffix! (-> Tree Label (Pairof Node Index)))
  (define (add-first-suffix! tree label)
    (: matched-at-node (-> Node (Pairof Node Index)))
    (define (matched-at-node node)
      (report-implicit-tree-constructed))
    (: matched-in-node (-> Node Index (Pairof Node Index)))
    (define (matched-in-node node offset)
      (report-implicit-tree-constructed))
    (: mismatched-at-node (-> Node Label Index (Pairof Node Index)))
    (define (mismatched-at-node node label label-offset)
      (define leaf
        (node-add-leaf! node (sublabel label label-offset)))
      (cons node label-offset))
    (: mismatched-in-node (-> Node Index Label Index (Pairof Node Index)))
    (define (mismatched-in-node node offset label label-offset)
      (define-values (joint leaf)
        (node-up-splice-leaf! node offset (sublabel label label-offset)))
      (cons joint label-offset))
    (define res (node-follow/k
                 (suffix-tree-root tree)
                 label
                 matched-at-node
                 matched-in-node
                 mismatched-at-node
                 mismatched-in-node))
    ;(when (void? res) (error "foo"))
    res)
  (: add-rest-suffixes! (-> Label Node Index Void))
  (define (add-rest-suffixes! label starting-node starting-offset)
    (add-rest-suffixes-loop!
     label
     (let ([i (label-length label)]) (unless (index? i) (error "ars")) i)
     (max starting-offset 1)
     1
     starting-node))
  (: add-rest-suffixes-loop! (-> Label Index Index Index Node Void))
  (define (add-rest-suffixes-loop! label N i j active-node)
    (when (< j N)
      (define-values (next-extension-node next-extension-offset i*)
        (find-next-extension-point/add-suffix-link! active-node label i j))
      (if i*
          (begin
            (let ([new-active-node
                   (extend-at-point! (begin (unless next-extension-node (error "bar bar bar")) next-extension-node)
                                     (begin (unless next-extension-offset (error "fofodofdof")) next-extension-offset)
                                     label i*)])
                  (try-to-set-suffix-edge! active-node new-active-node)
                  (add-rest-suffixes-loop!
                   label
                   N
                   (let ([num (max i* (add1 j))]) (unless (index? num) (error "foo")) num)
                   (let ([num (add1 j)]) (unless (index? num) (error "foo")) num)
                   new-active-node)))
          (begin (report-implicit-tree-constructed)
                 (void)))))
  (: report-implicit-tree-constructed (-> (Pairof Node Index)))
  (define (report-implicit-tree-constructed)
    (cons dummy-node 0))
  (do-construction! tree label))

;; -- from suffixtree.rkt

(provide tree-add!)
(define tree-add! suffix-tree-add!)
