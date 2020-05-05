#lang typed/racket

;; Utility Functions

(define-type Probability Nonnegative-Real)
;; constraint [0,1]

(provide
 ;Probability
 ;; ---
 sum
 relative-average
 choose-randomly)

 (: sum (-> [Listof Real] Real))
 (: relative-average (-> [Listof Real] Real Real))
 (: choose-randomly
  (->* [[Listof Probability] Natural] [(U False Real)] [Listof Natural]))

;; =============================================================================

(define (sum l)
  (apply + l))


(define (relative-average l w)
  (exact->inexact
   (/ (sum l)
      w (length l))))

;; -----------------------------------------------------------------------------

(define (choose-randomly probabilities speed (q #false))
  (define %s (accumulated-%s probabilities))
  (for/list ([n (in-range speed)])
    [define r (or q (random))]
    ;; population is non-empty so there will be some i such that ...
    (let loop : Natural ([%s : [Listof Real] %s])
      (cond
        [(< r (first %s)) 0]
        [else (add1 (loop (rest %s)))]))
    #;
    (for/last ([p (in-naturals)] [% (in-list %s)] #:final (< r %)) p)))

(: accumulated-%s (-> [Listof Probability] [Listof Real]))
;; [Listof Probability] -> [Listof Probability]
;; calculate the accumulated probabilities

(define (accumulated-%s probabilities)
  (define total (sum probabilities))
  (let relative->absolute : [Listof Real]
    ([payoffs : [Listof Real] probabilities][so-far : Real #i0.0])
    (cond
      [(empty? payoffs) '()]
      [else (define nxt (+ so-far (first payoffs)))
            ({inst cons Real Real}
             (/ nxt total) (relative->absolute (rest payoffs) nxt))])))
