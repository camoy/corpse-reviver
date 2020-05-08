#lang racket/base

(require (only-in "zombie.rkt"
  w0
  world-on-mouse
  world-on-tick
))

;; =============================================================================

(define (replay w0 hist)
 (let loop ((w  w0)
            (h  hist))
  (cond
   [(null? h)
    (void)]
   [(not (list? (car h)))
    (error "input error")]
   [else
    (define m (caar h))
    (define as (cdar h))
    (case m
     ;; no rendering
     [(to-draw stop-when)
       (loop w (cdr h))]
     [(on-mouse)
      (define r (apply (world-on-mouse w) (if (real-real-string-list? as)
                                              as (error "cast error"))))
      (loop r (cdr h))]
     [(on-tick)
      (define r ((world-on-tick w)))
      (loop r (cdr h))])])))

(define (real-real-string-list? x)
  (and (pair? x)
       (pair? (cdr x))
       (pair? (cdr (cdr x)))
       (null? (cdr (cdr (cdr x))))
       (real? (car x))
       (real? (car (cdr x)))
       (string? (car (cdr (cdr x))))))

(define DATA
  (with-input-from-file "../base/zombie-hist.rktd" read))

(define (main hist)
  (cond
   [(list? hist)
    (for ((i (in-range 100)))
      (replay w0 hist))]
   [else
    (error "bad input")]))

(time (main DATA))
