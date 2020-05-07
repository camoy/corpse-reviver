#lang racket/base

(require racket/contract/base
         racket/match)
(require "structs.rkt")

(provide/contract
 [find-utc-offset   (-> (vectorof interval?) real? tzoffset?)]
 [find-local-offset (-> (vectorof interval?) real? (or/c tzoffset? tzgap? tzoverlap?))])

(define (find-utc-offset intervals seconds)
  (match (binary-search intervals seconds (λ (x) 0) 0 (vector-length intervals) #f #f)
    [`#s(present ,idx)
     (interval-offset (vector-ref intervals idx))]))
    
(define (find-local-offset intervals seconds)
  (define (in-interval? n)
    (and (>= n 0)
         (< n (vector-length intervals))
         (eq? '= (interval-cmp (vector-ref intervals n) seconds tzoffset-utc-seconds))))
  
  (define (offset-at n)
    (interval-offset (vector-ref intervals n)))
  
  (match (binary-search intervals seconds tzoffset-utc-seconds 0 (vector-length intervals) #f #f)
    [`#s(present ,idx)
     ;; could be an overlap
     (cond [(in-interval? (sub1 idx))
            (tzoverlap (offset-at (sub1 idx))
                       (offset-at idx))]
           [(in-interval? (add1 idx))
            (tzoverlap (offset-at idx)
                       (offset-at (add1 idx)))]
           [else
            (offset-at idx)])]
    [`#s(absent ,idx)
     ;; gap
     (match (vector-ref intervals idx)
       [(interval t _ o)
        (tzgap t (offset-at (sub1 idx)) o)])]))

 
(define (binary-search intervals t offset->delta start end last best)
  (define i (quotient (+ end start) 2))
  
  (cond [(or (= start end)
             (eq? i last))
         `#s(absent ,best)]
        [else
         (case (interval-cmp (vector-ref intervals i) t offset->delta)
           [(=) `#s(present ,i)]
           [(<) (binary-search intervals t offset->delta start i i i)]
           [(>) (binary-search intervals t offset->delta i end i (add1 i))])]))

(define (interval-cmp int t offset->delta)
  (match int
    [(interval t1 t2 off)
     (define delta (offset->delta off))
     (define lo (+ t1 delta))
     (define hi (+ t2 delta))
     
     (cond [(< t lo)  '<]
           [(>= t hi) '>]
           [else      '=])]))
           