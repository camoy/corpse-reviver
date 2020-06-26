#lang typed/racket/base

(require corpse-reviver/require-typed-check)

(require/typed/check "streams.rkt"
  [#:struct stream ([first : Natural]
                    [rest : (-> stream)])]
  [make-stream (-> Natural (-> stream) stream)]
  [stream-unfold (-> stream (values Natural stream))]
  [stream-get (-> stream Natural Natural)]
  [stream-take (-> stream Natural (Listof Natural))])

;;--------------------------------------------------------------------------------------------------

;; `count-from n` Build a stream of integers starting from `n` and iteratively adding 1
(: count-from (-> Natural stream))
(define (count-from n)
  (make-stream n (lambda () (count-from (add1 n)))))

;; `sift n st` Filter all elements in `st` that are equal to `n`.
;; Return a new stream.
(: sift (-> Natural stream stream))
(define (sift n st)
  (define-values (hd tl) (stream-unfold st))
  (cond [(= 0 (modulo hd n)) (sift n tl)]
        [else (make-stream hd (lambda () (sift n tl)))]))

;; `sieve st` Sieve of Eratosthenes
(: sieve (-> stream stream))
(define (sieve st)
  (define-values (hd tl) (stream-unfold st))
  (make-stream hd (lambda () (sieve (sift hd tl)))))

;; stream of prime numbers
(: primes stream)
(define primes (sieve (count-from 2)))

(: N-1 Natural)
(define N-1 6666)

(: main (-> Void))
(define (main)
  (void (stream-get primes N-1)))

(time (main))
