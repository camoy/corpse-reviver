#lang typed/racket/base

(require (only-in racket/fixnum fx<= fxmax fxmodulo)
         (only-in racket/vector vector-append)
         (only-in racket/string string-join)
         (only-in racket/list empty? first rest)
         corpse-reviver/require-typed-check
         "typed-data.rkt")

(require/typed/check "array-struct.rkt"
  [array-strict? (-> Array Boolean)]
  [array-default-strict! (-> Array Void)]
  [array-shape (-> Array Indexes)]
  [array-size (-> Array Integer)]
  [unsafe-array-proc (-> Array (-> Indexes Float))]
  [unsafe-build-array (-> Indexes (-> Indexes Float) Array)])

(require/typed/check "array-utils.rkt"
  [make-thread-local-indexes (-> Integer (-> Indexes))])

(provide array-broadcasting
         array-broadcast
         array-shape-broadcast)

(: array-broadcasting (Boxof (U #f #t 'permissive)))
(define array-broadcasting (box #t))

(: shift-stretch-axes (-> Array Indexes Array))
(define (shift-stretch-axes arr new-ds)
  (define old-ds (array-shape arr))
  (define old-dims (vector-length old-ds))
  (define new-dims (vector-length new-ds))
  (define shift
    (let ([shift  (- new-dims old-dims)])
      (cond [(index? shift)  shift]
            [else  (error 'array-broadcast
                          "cannot broadcast to a lower-dimensional shape; given ~e and ~e"
                          arr new-ds)])))
  (define old-js (make-thread-local-indexes old-dims))
  (define old-f (unsafe-array-proc arr))
  (unsafe-build-array
   new-ds
   (λ: ([new-js : Indexes])
     (let ([old-js  (old-js)])
       (let: loop : Float ([k : Integer  0])
         (cond [(k . < . old-dims)
                (define new-jk (vector-ref new-js (+ k shift)))
                (define old-dk (vector-ref old-ds k))
                (define old-jk (fxmodulo new-jk old-dk))
                (vector-set! old-js k old-jk)
                (loop (+ k 1))]
               [else  (old-f old-js)]))))) )

(: array-broadcast (-> Array Indexes Array))
(define (array-broadcast arr ds)
  (cond [(equal? ds (array-shape arr))  arr]
        [else  (define new-arr (shift-stretch-axes arr ds))
               (if (or (array-strict? arr) ((array-size new-arr) . fx<= . (array-size arr)))
                   new-arr
                   (begin (array-default-strict! new-arr) new-arr))]))

(: shape-insert-axes (Indexes Integer -> Indexes))
(define (shape-insert-axes ds n)
  (vector-append ((inst make-vector Integer) n 1) ds))

(: shape-permissive-broadcast (Indexes Indexes Integer (-> Nothing) -> Indexes))
(define (shape-permissive-broadcast ds1 ds2 dims fail)
  (define: new-ds : Indexes (make-vector dims 0))
  (let loop ([#{k : Integer} 0])
    (cond [(k . < . dims)
           (define dk1 (vector-ref ds1 k))
           (define dk2 (vector-ref ds2 k))
           (vector-set!
            new-ds k
            (cond [(or (= dk1 0) (= dk2 0))  (fail)]
                  [else  (fxmax dk1 dk2)]))
           (loop (+ k 1))]
          [else  new-ds])))

(: shape-normal-broadcast (Indexes Indexes Integer (-> Nothing) -> Indexes))
(define (shape-normal-broadcast ds1 ds2 dims fail)
  (define: new-ds : Indexes (make-vector dims 0))
  (let loop ([#{k : Integer} 0])
    (cond [(k . < . dims)
           (define dk1 (vector-ref ds1 k))
           (define dk2 (vector-ref ds2 k))
           (vector-set!
            new-ds k
            (cond [(= dk1 dk2)  dk1]
                  [(and (= dk1 1) (dk2 . > . 0))  dk2]
                  [(and (= dk2 1) (dk1 . > . 0))  dk1]
                  [else  (fail)]))
           (loop (+ k 1))]
          [else  new-ds])))

(: shape-broadcast2 (Indexes Indexes (-> Nothing) (U #f #t 'permissive) -> Indexes))
(define (shape-broadcast2 ds1 ds2 fail broadcasting)
  (cond [(equal? ds1 ds2)  ds1]
        [(not broadcasting)  (fail)]
        [else
         (define dims1 (vector-length ds1))
         (define dims2 (vector-length ds2))
         (define n (- dims2 dims1))
         (let-values ([(ds1 ds2 dims)
                       (cond [(n . > . 0)  (values (shape-insert-axes ds1 n) ds2 dims2)]
                             [(n . < . 0)  (values ds1 (shape-insert-axes ds2 (- n)) dims1)]
                             [else         (values ds1 ds2 dims1)])])
           (if (eq? broadcasting 'permissive)
               (shape-permissive-broadcast ds1 ds2 dims fail)
               (shape-normal-broadcast ds1 ds2 dims fail)))]))

(: array-shape-broadcast (case-> ((Listof Indexes) -> Indexes)
                                 ((Listof Indexes) (U #f #t 'permissive) -> Indexes)))
(define (array-shape-broadcast dss [broadcasting (unbox array-broadcasting)])
  (define (fail) (error 'array-shape-broadcast
                        "incompatible array shapes (array-broadcasting ~v): ~a"
                        broadcasting
                        (string-join (map (λ (ds) (format "~e" ds)) dss) ", ")))
  (cond [(empty? dss)  #()]
        [else  (for/fold ([new-ds  (first dss)]) ([ds  (in-list (rest dss))])
                 (shape-broadcast2 new-ds ds fail broadcasting))]))
