#lang racket

(require "data.rkt"
         "const.rkt"
         "handlers.rkt"
         "motion.rkt")

(define (replay w0 hist)
  (reset!)
  (let loop ((w w0) (h hist))
    (if (empty? h)
        w
        (let ()
          (loop
           (match (car h)
             [`(on-key ,(? string? ke))
              (handle-key w ke)]
             [`(on-tick)
              (world->world w)]
             [`(stop-when)
              (game-over? w)
              w])
           (cdr h)))))
  (void))

(define DATA (with-input-from-file "../base/snake-hist.rktd" read))
(define LOOPS 200)

(define (main hist)
  (define w0 (WORLD))
  (cond [(list? hist)
         (for ((_i (in-range LOOPS)))
           (replay w0 hist))]
        [else
         (error "bad input")]))

(time (main DATA))
