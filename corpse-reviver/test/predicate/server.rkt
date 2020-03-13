#lang typed/racket/base

(define is-number? (make-predicate Number))
(define-predicate is-number*? Integer)

(provide typed-sum)

(: typed-sum (-> (Listof Number) Number))
(define (typed-sum xs)
  (cond
    [(null? xs) 0]
    [(pair? xs)
     (if (and (is-number? (car xs))
              (is-number*? (car xs)))
         (+ (car xs) (typed-sum (cdr xs)))
         (error "this should be impossible"))]))
