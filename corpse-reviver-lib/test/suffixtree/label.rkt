#lang typed/racket/base

(require (rename-in "typed-data.rkt" [make-label -make-label]))

;; Label implementation.  Labels are like strings, but also allow for
;; efficient shared slicing.
;;
;; TODO later: see if we can generalize labels to be more than just
;; text strings.  It might be useful to have them as arbitrary
;; elements, perhaps as vectors?

;; label-element? object -> true
;; Every value is considered to be a possible label-element.
(: label-element? (-> Any Boolean))
(define (label-element? obj) #t)

;; When comparing label elements, we use equal?.
(: label-element-equal? (-> Any Any Boolean))
(define label-element-equal? equal?)

(provide
        make-label
        label-element?
        label-element-equal?
        string->label
        string->label/with-sentinel
        vector->label
        vector->label/with-sentinel
        label->string
        label->string/removing-sentinel
        label->vector
        label-length
        label-ref
        sublabel
        sublabel!
        label-prefix?
        label-equal?
        label-empty?
        label-copy
        label-ref-at-end?
        label-source-id
        label-same-source?
        label-source-eq?)


;;make-label: label-element -> label
;;Constructs a new label from either a string or a vector of things.
(: make-label (-> (U String (Vectorof (U Char Symbol))) Label))
(define (make-label label-element)
 (cond ((string? label-element) (string->label label-element))
       ((vector? label-element) (vector->label label-element))
       (else
        (error 'make-label "Don't know how to make label from ~S" label-element))))

(: make-sentinel (-> Symbol))
(define (make-sentinel)
 (gensym 'sentinel))

(: sentinel? (-> Any Boolean))
(define (sentinel? datum)
 (symbol? datum))

;; vector->label vector
;; Constructs a new label from the input vector.
(: vector->label (-> (Vectorof (U Char Symbol)) label))
(define (vector->label vector)
  (-make-label (vector->immutable-vector vector)
             0 (vector-length vector)))


;; vector->label vector
;; Constructs a new label from the input vector, with a sentinel
;; symbol at the end.
(: vector->label/with-sentinel (-> (Vectorof Char) label))
(define (vector->label/with-sentinel vector)
  (: N Index)
  (define N (vector-length vector))
  (: V (Vectorof (U Char Symbol)))
  (define V (make-vector (add1 N) (make-sentinel)))
  (let loop ((i 0))
    (if (< i N)
        (begin (vector-set! V i (vector-ref vector i))
               (loop (add1 i)))
        (vector->label V))))


;;string->label: string -> label
;;Constructs a new label from the input string.
(: string->label (-> String label))
(define (string->label str)
  (vector->label (list->vector (string->list str))))


;; string->label/with-sentinel: string -> label
;; Constructs a new label from the input string, attaching a unique
;; sentinel symbol at the end of the label.
;;
;; Note: this label can not be converted in whole back to a string:
;; the sentinel character interferes with string concatenation
(: string->label/with-sentinel (-> String label))
(define (string->label/with-sentinel str)
  (vector->label/with-sentinel (list->vector (string->list str))))

;; label-length: label -> number?
;; Returns the length of the label.
(: label-length (-> label Index))
(define (label-length label)
  (define len (- (label-j label) (label-i label)))
  (unless (index? len) (error "label-length"))
  len)


; label-ref: label number? -> char
; Returns the kth element in the label.
(: label-ref (-> label Integer (U Symbol Char)))
(define (label-ref label k)
  (unless (index? k) (error "label ref INDEX"))
  (vector-ref (label-datum label) (+ k (label-i label))))

;; sublabel: label number number -> label
;; Gets a slice of the label on the half-open interval [i, j)
(: sublabel (case-> (-> label Index label)
                    (-> label Index Index label)))
(define sublabel
  (case-lambda
    ((label i)
     (sublabel label i (label-length label)))
    ((label i j)
     (unless (<= i j)
       (error 'sublabel "illegal sublabel [~a, ~a]" i j))
     (-make-label (label-datum label)
                 (+ i (label-i label))
                 (+ j (label-i label))))))

;; sublabel!: label number number -> void
;; destructively sets the input label to sublabel.
(: sublabel! (case-> (-> label Index Void)
                     (-> label Index Index Void)))
(define sublabel!
  (case-lambda
    ((label i)
     (sublabel! label i (label-length label)))
    ((label i j)
     (begin
       ;; order dependent code ahead!
       (set-label-j! label (+ j (label-i label)))
       (set-label-i! label (+ i (label-i label)))
       (void)))))


;; label-prefix?: label label -> boolean
;; Returns true if the first label is a prefix of the second label
(: label-prefix? (-> label label Boolean))
(define (label-prefix? prefix other-label)
  (let ((m (label-length prefix))
        (n (label-length other-label)))
    (if (> m n)                       ; <- optimization: prefixes
					; can't be longer.
        #f
        (let loop ((k 0))
          (if (= k m)
              #t
              (and (index? k)
                   (equal? (label-ref prefix k) (label-ref other-label k))
                   (loop (add1 k))))))))


;; label-equal?: label label -> boolean
;; Returns true if the two labels are equal.
(: label-equal? (-> label label Boolean))
(define (label-equal? l1 l2)
  (and (= (label-length l1) (label-length l2))
       (label-prefix? l1 l2)))


;; label-empty?: label -> boolean
;; Returns true if the label is considered empty
(: label-empty? (-> label Boolean))
(define (label-empty? label)
  (>= (label-i label) (label-j label)))


;; label->string: label -> string
;; Extracts the string that the label represents.
;; Precondition: the label must have originally come from a string.
;; Note: this operation is expensive: don't use it except for debugging.
(: label->string (-> label String))
(define (label->string label)
  (: V (Vectorof (U Char Symbol)))
  (define V (label->vector label))
  (: L (Listof Char))
  (define L (for/list : (Listof Char)
                      ([c : (U Char Symbol) (in-list (vector->list V))])
              (unless (char? c) (error "label->string invariant broken"))
              c))
  (list->string L))

(: label->string/removing-sentinel (-> label String))
(define (label->string/removing-sentinel label)
  (let* ([ln (label-length label)]
         [N (if (and (> ln 0) (sentinel? (label-ref label (sub1 ln))))
                (sub1 ln)
                ln)])
    (build-string N (lambda ([i : Integer])
                      (unless (index? i) (error "label->string 1"))
                      (let ([val (label-ref label i)])
                        (unless (char? val) (error "label->string 2"))
                        val)))))

;; label->vector: label -> vector
;; Extracts the vector that the label represents.
;; Note: this operation is expensive: don't use it except for debugging.
(: label->vector (-> label (Vectorof (U Char Symbol))))
(define (label->vector label)
  (: N Integer)
  (define N (label-length label))
  (: buffer (Vectorof (U Char Symbol)))
  (define buffer (make-vector N 'X));;'X is a placeholder
    (let loop ((i 0))
      (if (and (< i N) (index? i))
          (begin
            (vector-set! buffer i (label-ref label i))
           (loop (add1 i)))
          (vector->immutable-vector buffer))))


;; label-copy: label->label
;; Returns a copy of the label.
(: label-copy (-> label label))
(define (label-copy label)
  (-make-label (label-datum label) (label-i label) (label-j label)))


;; label-ref-at-end?: label number -> boolean
(: label-ref-at-end? (-> label Integer Boolean))
(define (label-ref-at-end? label offset)
  (= offset (label-length label)))


;; label-source-id: label -> number
(: label-source-id (-> label Integer))
(define (label-source-id label)
  (eq-hash-code (label-datum label)))

;; label-same-source?: label label -> boolean
(: label-same-source? (-> label label Boolean))
(define (label-same-source? label-1 label-2)
  (eq? (label-datum label-1) (label-datum label-2)))

;; --- from suffixtree.rkt
(define label-source-eq? label-same-source?)
