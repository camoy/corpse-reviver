#lang typed/racket/base

(require corpse-reviver/require-typed-check
         "typed-data.rkt")

(require/typed/check "array-struct.rkt"
  [array-size (-> Array Integer)]
  [make-array (-> In-Indexes Flonum Array)]
  [build-array (-> In-Indexes (-> Indexes Float) Array)]
  [unsafe-vector->array (-> Indexes (Vectorof Float) Mutable-Array)])

(require/typed/check "array-utils.rkt"
  [array-shape-size (-> Indexes Integer)]
  [check-array-shape (-> In-Indexes (-> Nothing) Indexes)])

(require/typed/check "array-transform.rkt"
  [array-append* ((Listof Array) -> Array)])

(require/typed/check "synth.rkt"
  [fs Natural]
  [seconds->samples (-> Float Integer)])

(provide drum)

(: random-sample (-> Float))
(define (random-sample) (- (* 2.0 (random)) 1.0))

;; Drum "samples" (Arrays of floats)
;; TODO compute those at compile-time
(: bass-drum Array)
(define bass-drum
  (let ()
    ;; 0.05 seconds of noise whose value changes every 12 samples
    (: n-samples Integer)
    (define n-samples           (seconds->samples 0.05))
    (: n-different-samples Integer)
    (define n-different-samples (quotient n-samples 12))
    (: ds* In-Indexes)
    (define ds* (vector n-samples))
    (: ds  Indexes)
    (define ds
      (check-array-shape ds*
                         (λ () (raise-argument-error 'name "Indexes" ds))))
    (: vs (Vectorof Flonum))
    (define vs (make-vector (array-shape-size ds) 0.0))
    (for ([i      : Natural (in-range n-different-samples)]
          [sample : Flonum  (in-producer random-sample)]
          #:when #t
          [j      : Natural (in-range 12)])
      (vector-set! vs (+ (* i 12) j) sample))
    (unsafe-vector->array ds vs)))

(: snare Array)
(define snare
  ;; 0.05 seconds of noise
  (let ([indexes : In-Indexes
                  (vector (seconds->samples 0.05))]
         [arr-gen : (-> Indexes Flonum)
                  (lambda ([x : Indexes]) (random-sample))])
    (build-array indexes arr-gen)))

;; limited drum machine
(: drum (-> Natural Pattern Natural Array))
(define (drum n pattern tempo)
  (: samples-per-beat Natural)
  (define samples-per-beat (quotient (* fs 60) tempo))
  (: make-drum (-> Array Natural Array))
  (define (make-drum drum-sample samples-per-beat)
    (array-append*
     (list drum-sample
           (make-array (vector (- samples-per-beat
                                  (array-size drum-sample)))
                       0.0))))
  (: O Array)
  (define O     (make-drum bass-drum samples-per-beat))
  (: X Array)
  (define X     (make-drum snare     samples-per-beat))
  (: pause Array)
  (define pause (make-array (vector samples-per-beat) 0.0))
  (array-append*
   (for*/list : (Listof Array) ([i : Integer (in-range n)]
                                        [beat : Drum-Symbol (in-list pattern)])
     (case beat
       [(X)  X]
       [(O)  O]
       [(#f) pause]))))
