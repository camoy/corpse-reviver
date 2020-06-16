#lang racket/base

;; Much of this comes from reading Dan Gusfield's Algorithms on
;; strings, trees, and sequences: computer science and computational
;; biology.

(require "structs.rkt"
  "untyped.rkt"
  (except-in "data.rkt" make-label)
  "label.rkt")

(define dummy-node (node (make-label "dummy") #f '() #f))


(provide skip-count)
;; skip-count: node label -> (values node number)
;;
;; Follows down the node using the skip-count rule until we exhaust
;; the label.  Assumes that there does exist a labeled path starting
;; from the node that exactly matches label.
(define (skip-count node label)
  (define l-l (label-length label))
  (unless (index? l-l) (error "skip-count"))
  (skip-count-helper node label 0 l-l))

;; Utility function for skip count, but also visible for those in
;; the know to skip-count from an arbitrary position in label.
(define (skip-count-helper node label k N)
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






(provide jump-to-suffix)
;; jump-to-suffix: node -> (values node (union boolean number))
;;
;; Given an internal node, jumps to the suffix from that node.
;; According to the theory of suffix trees, such a node will exist
;; in the tree if we follow the Ukkonen construction.  If we had to
;; go up a few characters, returns the number of chars at the suffix
;; end that need to be compared to get the real suffix.

;; If we hit the root, that offset is #f to indicate that we have to
;; start searching the suffix from scratch.
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
(define (try-to-set-suffix-edge! from-node to-node)
  (when (not (node-suffix-link from-node))
    (set-node-suffix-link! from-node to-node)))



(provide find-next-extension-point/add-suffix-link!)
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
(define (find-next-extension-point/add-suffix-link! node label initial-i j)
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
   (define (loop-first i)
     (loop-general i (lambda (skipped-node
                              skip-offset)
                       (when (node-position-at-end? skipped-node skip-offset)
                         (try-to-set-suffix-edge! node skipped-node)))))
   (define (loop-rest i)
     (loop-general i (lambda (skipped-node skip-offset)
                       (void))))
   (define (loop-general i first-shot)
     (cond [(>= i N) (values #f #f #f)]
           [else 
            (define-values (skipped-node skipped-offset)
              (skip-count-helper suffix-node label K i))
            (first-shot skipped-node skipped-offset)
             (if (node-position-at-end? skipped-node skipped-offset)
                 (find-extension-at-end! skipped-node skipped-offset i)
                 (find-extension-in-edge skipped-node skipped-offset i))]))
   (define (find-extension-in-edge skipped-node skip-offset i)
     (cond [(label-element-equal?
             (label-ref label i)
             (label-ref (node-up-label skipped-node) skip-offset))
            (let ([n (add1 i)]) (unless (index? n) (error "find-next")) (loop-rest n))]
           [else (values skipped-node skip-offset i)]))
   (define (find-extension-at-end! skipped-node skip-offset i)
     (cond [(node-find-child skipped-node (label-ref label i))
            (let ([n (add1 i)]) (unless (index? n) (error "find-next")) (loop-rest n))]
           [else (values skipped-node skip-offset i)]))
   (loop-first initial-i))


(provide extend-at-point!)
;; extend-at-point!: node number label number -> node
(define (extend-at-point! node offset label i)
  (define (main-logic node offset label i)
    (if (should-extend-as-leaf? node offset)
        (attach-as-leaf! node label i)
        (splice-with-internal-node! node offset label i)))
  (define (should-extend-as-leaf? node offset)
    (node-position-at-end? node offset))
  (define (attach-as-leaf! node label i)
    (define leaf (node-add-leaf! node (sublabel label i)))
    node)
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
(define (suffix-tree-add! tree label)
  (define (do-construction! tree label)
    (define pr (add-first-suffix! tree label))
    (define starting-node (car pr))
    (define starting-offset (cdr pr))
    (add-rest-suffixes! label starting-node starting-offset))
  (define (add-first-suffix! tree label)
    (define (matched-at-node node)
      (report-implicit-tree-constructed))
    (define (matched-in-node node offset)
      (report-implicit-tree-constructed))
    (define (mismatched-at-node node label label-offset)
      (define leaf
        (node-add-leaf! node (sublabel label label-offset)))
      (cons node label-offset))
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
    res)
  (define (add-rest-suffixes! label starting-node starting-offset)
    (add-rest-suffixes-loop!
     label
     (let ([i (label-length label)]) (unless (index? i) (error "ars")) i)
     (max starting-offset 1)
     1
     starting-node))
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
  (define (report-implicit-tree-constructed)
    (cons dummy-node 0))
  (do-construction! tree label))

;; -- from suffixtree.rkt

(provide tree-add!)
(define tree-add! suffix-tree-add!)


