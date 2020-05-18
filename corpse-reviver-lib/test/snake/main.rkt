#lang typed/racket

(require corpse-reviver/require-typed-check
         "data-adaptor.rkt")
(require/typed/check "const.rkt"
                     [WORLD (-> World)])
(require/typed/check "motion.rkt"
                     [reset!           (-> Void)]
                     [world->world     (World . -> . World)])
(require/typed/check "handlers.rkt"
                     [handle-key (World String . -> . World)]
                     [game-over? (World . -> . Boolean)])

(: replay (-> World (Listof Any) Void))
(define (replay w0 hist)
  (reset!)
  (let loop ((w : World w0)
             (h : (Listof Any) hist))
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
(define LOOPS 10)

(: main (-> Any Void))
(define (main hist)
  (define w0 (WORLD))
  (cond [(list? hist)
         (for ([i (in-range LOOPS)])
           (replay w0 hist))]
        [else
         (error "bad input")]))

(time (main DATA))
