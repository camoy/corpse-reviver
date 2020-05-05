#lang typed/racket/base
(require scv-cr/require-typed-check
         (except-in "typed-data.rkt" label)
         racket/list)

(require/typed/check
 "label.rkt"
 [label (-> (U String (Vectorof (U Char Symbol))) Label)]
 [label-element-equal? (-> Any Any Boolean)]
 [label-length (-> Label Index)]
 [label-ref (-> Label Integer (U Symbol Char))]
 [sublabel (case-> (-> Label Index Label)
                    (-> Label Index Index Label))]
 [label-copy (-> Label Label)]
 [label-ref-at-end? (-> Label Integer Boolean)]
 [label->string (-> Label String)]
 [label-source-eq? (-> Label Label Boolean)]
 [string->label (-> String Label)]
 [string->label/with-sentinel (-> String Label)]
 [vector->label (-> (Vectorof (U Char Symbol)) Label)]
 [vector->label/with-sentinel (-> (Vectorof Char) Label)]
 [label-same-source? (-> Label Label Boolean)]
 )

(provide
 tree?
 tree-root
 make-tree
 new-suffix-tree
 node-find-child
 node-root?
 node-position-at-end?
 node-add-leaf!
 node-up-splice-leaf!
 node-follow/k)


;; new-suffix-tree: void -> suffix-tree
;; Builds a new empty suffix-tree.
(: new-suffix-tree (-> Tree))
(define (new-suffix-tree)
  (suffix-tree
   ;; The root node has no label, no parent, an empty list of
   ;; children.  Its suffix link is invalid, but we set it to #f.
   (let ((root (node (label (make-vector 0 'X)) #f (list) #f)))
     root)))

(: node-root? (-> Node Boolean))
(define (node-root? node)
  (eq? #f (node-parent node)))


;; node-add-leaf!: node label -> node
;; Attaches a new leaf node to an internal node.  Returns thew new leaf.
(: node-add-leaf! (-> Node Label Node))
(define (node-add-leaf! n l)
  (let ((leaf (node l n (list) #f)))
    (node-add-child! n leaf)
    leaf))


;; node-add-child!: node node -> void
;; Adds to the node list.
(: node-add-child! (-> Node Node Void))
(define (node-add-child! node child)
  (set-node-children! node (cons child (node-children node))))


;; node-remove-child!: node node -> void
;; Removes child node.
(: node-remove-child!(-> Node Node Void))
(define (node-remove-child! node child)
  (set-node-children! node (remq child (node-children node))))


;; Constructor for children is a list at the moment.  TODO: change
;; children representation based on density?
(define children-list list)


;; node-leaf?: node -> boolean
;; Returns true if node is a leaf
(: node-leaf? (-> Node Boolean))
(define (node-leaf? node)
  (empty? (node-children node)))


;; node-find-child: node label-element -> (union node #f)
;;
;; Finds the first child node whose up-label starts with (label-ref
;; label 0).  If none can be found, returns #f.
(: node-find-child (-> Node Any (U Node #f)))
(define (node-find-child node label-element)
  (: loop (-> (Listof Node) (U Node #f)))
  (define (loop children)
    (cond ((null? children) #f)
          ((label-element-equal? label-element (label-ref (node-up-label (first children)) 0))
           (first children))
          (else
           (loop (rest children)))))
  (loop (node-children node)))


;; node-up-split!: node number -> node
;; Introduces a new node that goes between this node and its parent.
(: node-up-split! (-> Node Index Node))
(define (node-up-split! n offset)
  (let* ((l (node-up-label n))
         (pre-label (sublabel l 0 offset))
         (post-label (sublabel l offset))
         (parent (node-parent n))
         (new-node (node pre-label parent (children-list n) #f)))
    (set-node-up-label! n post-label)
    (unless parent (error "node-up-split!"))
    (node-remove-child! parent n)
    (set-node-parent! n new-node)
    (node-add-child! parent new-node)
    new-node))


;; node-up-splice-leaf!: node offset label -> (values node node)
;;
;; Adds a new leaf at a splice joint between the node and its
;; parent.  Returns both the joint and the leaf.
(: node-up-splice-leaf! (-> Node Index Label (values Node Node)))
(define (node-up-splice-leaf! node offset leaf-label)
  (let* ((split-node (node-up-split! node offset))
         (leaf (node-add-leaf! split-node leaf-label)))
    (values split-node leaf)))


;; tree-contains?: tree label -> boolean
;; Returns true if the tree contains the given label.
(: tree-contains? (-> Tree Label Boolean))
(define (tree-contains? tree label)
  (node-follow/k (suffix-tree-root tree)
                 label
                 (lambda args #t)
                 (lambda args #t)
                 (lambda args #f)
                 (lambda args #f)))


;; node-follow/k: node label (node -> A)
;;                           (node number -> B)
;;                           (node label number -> C)
;;                           (node number label number -> D)
;;                    -> (union A B C D)
;; 
;; Traverses the node's edges along the elements of the input label.
;; Written in continuation-passing-style for leakage containment.
;; One of the four continuation arguments will be executed.
(: node-follow/k (All (A B C D)
                      (-> Node
                          Label
                          (-> Node A)
                          (-> Node Index B)
                          (-> Node Label Index C)
                          (-> Node Index Label Index D)
                          (U A B C D))))
(define (node-follow/k node
                       original-label
                       matched-at-node/k
                       matched-in-edge/k
                       mismatched-at-node/k
                       mismatched-in-edge/k)
  (: EDGE/k (-> Node Label Index (U A B C D)))
  (define (EDGE/k node label label-offset)
    (: up-label Label)
    (define up-label (node-up-label node))
    (let loop ((k 0))
      (define k+label-offset (+ k label-offset))
      (cond
       ((= k (label-length up-label))
        (unless (index? k+label-offset) (error "node/folllowd"))
        (NODE/k node label k+label-offset))
       ((= k+label-offset (label-length label))
        (unless (index? k) (error "node/followk"))
        (matched-in-edge/k node k))
       ((label-element-equal? (label-ref up-label k)
                              (label-ref label k+label-offset))
        (loop (add1 k)))
       (else
        (unless (and (index? k)
                     (index? k+label-offset)) (error "node-follow/k mismatched fail"))
        (mismatched-in-edge/k node k label
                              k+label-offset)))))
  (: NODE/k (-> Node Label Index (U A B C D)))
  (define (NODE/k node label label-offset)
    (if (= (label-length label) label-offset)
        (matched-at-node/k node)
        (let ([child (node-find-child node (label-ref label label-offset))])
          (if child
              (EDGE/k child label label-offset)
              (mismatched-at-node/k node label label-offset)))))
  (NODE/k node (label-copy original-label) 0))


;; node-position-at-end?: node number -> boolean
;;
;; Returns true if the position defined by node and the up-label
;; offset are pointing at the end of the node.
(: node-position-at-end? (-> Node Index Boolean))
(define (node-position-at-end? node offset)
  (label-ref-at-end? (node-up-label node) offset))

;; --- from suffixtree.rkt

(define tree? suffix-tree?)

(define make-tree new-suffix-tree)

(define tree-root suffix-tree-root)

