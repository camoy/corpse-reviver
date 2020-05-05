#lang typed/racket

(require scv-cr/require-typed-check
         "data-adaptor.rkt")
(require/typed/check "cut-tail.rkt"
                     [cut-tail ((NEListof Posn) . -> . (Listof Posn))])

;; next-head : Posn Direction -> Posn
;; Compute next position for head.
(: next-head : (Posn Dir . -> . Posn))
(define (next-head seg dir)
  (cond [(equal? "right" dir) (posn (add1 (posn-x seg)) (posn-y seg))]
        [(equal? "left" dir)  (posn (sub1 (posn-x seg)) (posn-y seg))]
        [(equal? "down" dir)  (posn (posn-x seg) (sub1 (posn-y seg)))]
        [else                 (posn (posn-x seg) (add1 (posn-y seg)))]))

;; snake-slither : Snake -> Snake
;; move the snake one step
(: snake-slither : (Snake . -> . Snake))
(define (snake-slither snk)
  (let ([d (snake-dir snk)])
    (snake d
           (cons (next-head (car (snake-segs snk))
                            d)
                 (cut-tail (snake-segs snk))))))

;; snake-grow : Snake -> Snake
;; Grow the snake one segment.
(: snake-grow : (Snake . -> . Snake))
(define (snake-grow snk)
  (let ([d (snake-dir snk)])
    (snake d
           (cons (next-head (car (snake-segs snk))
                            d)
                 (snake-segs snk)))))

(provide
 snake-slither
 snake-grow)
