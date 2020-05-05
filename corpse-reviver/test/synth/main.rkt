#lang typed/racket/base

(require scv-cr/require-typed-check
         "typed-data.rkt")

(require/typed/check "sequencer.rkt"
  [note (-> Symbol Natural Natural (Pairof Natural Natural))]
  [sequence (-> Natural (Listof (Pairof (U Natural #f) Natural)) Natural (-> Float (-> Indexes Float)) Array)])

(require/typed/check "drum.rkt"
  [drum (-> Natural Pattern Natural Array)])

(require/typed/check "mixer.rkt"
  [mix (-> (Listof Weighted-Signal) Array)])

(require/typed/check "synth.rkt"
  [emit (-> Array (Vectorof Integer))]
  [sawtooth-wave (-> Float (-> Indexes Float))])

(define LOOPS 6)

;; Test from Vincent's repo.
(: large-test (-> (Vectorof Integer)))
(define (large-test)
 (emit
  (mix
   (list
    (list
     (sequence LOOPS (list
     (note 'C 5 1)
     (cons #f 1)
     (note 'C 5 1)
     (cons #f 1)
     (note 'A# 4 1)
     (cons #f 1)
     (note 'C 5 1)
     (cons #f 3)
     (note 'G 4 1)
     (cons #f 3)
     (note 'G 4 1)
     (cons #f 1)
     (note 'C 5 1)
     (cons #f 1)
     (note 'F 5 1)
     (cons #f 1)
     (note 'E 5 1)
     (cons #f 1)
     (note 'C 5 1)
     (cons #f 9))
               380 sawtooth-wave)
     1)
    (list
     (drum 8 '(O #f #f #f X #f #f #f) 380)
     1)))))

;; Small test, for development
(: small-test (-> (Vectorof Integer)))
(define (small-test)
  (emit
   (mix
    (list
     (list
      (sequence 1 (list
                   (note 'C 5 1)
                   (cons #f 1)
                   (note 'C 5 1))
                1200 sawtooth-wave)
      1)
     (list
      (drum 1 '(O #f #f #f X) 1200)
      1)))))

(define (mid-test)
  (emit
   (mix
    (list
    (list
     (sequence 1 (list
                  (note 'D 0 1)
                  (note 'D 5 1)
                  (cons #f 1)
                  (note 'D 3 1)
                  (note 'D 8 1)
                  (cons #f 1)
                  (note 'D 5 1)
                  (note 'D 10 1)
                  (cons #f 1)
                  (note 'D 0 1)
                  (note 'D 5 1)
                  (cons #f 1)
                  (note 'D 3 1)
                  (note 'D 8 1)
                  (cons #f 1)
                  (note 'D 6 1)
                  ;; (note 'D 11 1)
                  (cons #f 1)
                  (note 'D 5 1)
                  (note 'D 10 1)
                  (cons #f 2)
                  (note 'D 0 1)
                  (note 'D 5 1)
                  (cons #f 1)
                  (note 'D 3 1)
                  (note 'D 8 1)
                  (cons #f 1)
                  (note 'D 5 1)
                  (note 'D 10 1)
                  (cons #f 1)
                  (note 'D 3 1)
                  (note 'D 8 1)
                  (cons #f 1)
                  (note 'D 0 1)
                  (note 'D 5 1))
               1200 sawtooth-wave)
     1)
    (list (drum 1 '(O #f X #f O #f X #f O #f X #f O O X X) 360)
          1)))))

(: main (-> Void))
(define (main)
  ;; (mid-test) ;; 37ms
  ;;(large-test) ;; 110ms
  (small-test) ;; 3ms
  (void))

(time (main))
