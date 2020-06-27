#lang typed/racket/base

(require corpse-reviver/require-typed-check
         "typed-data.rkt")

(require/typed "array-struct.rkt"
  [build-array (-> Indexes (-> Indexes Flonum) Array)])

(require/typed "array-transform.rkt"
  [array-append* ((Listof Array) -> Array)])

(require/typed "synth.rkt"
  [fs Natural])

(require/typed "mixer.rkt"
  [mix (-> Weighted-Signal * Array)])

(provide sequence note)

;; details at http://www.phy.mtu.edu/~suits/notefreqs.html
(: note-freq (-> Natural Float))
(define (note-freq note)
  ;; A4 (440Hz) is 57 semitones above C0, which is our base.
  (: res Real)
  (define res (* 440 (expt (expt 2 1/12) (- note 57))))
  (if (flonum? res) res (error "not real")))

;; A note is represented using the number of semitones from C0.
(: name+octave->note (-> Symbol Natural Natural))
(define (name+octave->note name octave)
  (+ (* 12 octave)
     (case name
       [(C) 0]
       [(C# Db) 1]
       [(D) 2]
       [(D# Eb) 3]
       [(E) 4]
       [(F) 5]
       [(F# Gb) 6]
       [(G) 7]
       [(G# Ab) 8]
       [(A) 9]
       [(A# Bb) 10]
       [(B) 11]
       [else 0])))

;; Single note.
(: note (-> Symbol Natural Natural (Pairof Natural Natural)))
(define (note name octave duration)
  (cons (name+octave->note name octave) duration))

;; Accepts notes or pauses, but not chords.
(: synthesize-note (-> (U #f Natural)
                       Natural
                       (-> Float (-> Indexes Float))
                       Array))
(define (synthesize-note note n-samples function)
  (build-array (vector n-samples)
               (if note
                   (function (note-freq note))
                   (lambda (x) 0.0)))) ; pause

;; repeats n times the sequence encoded by the pattern, at tempo bpm
;; pattern is a list of either single notes (note . duration) or
;; chords ((note ...) . duration) or pauses (#f . duration)
(: sequence (-> Natural
                (Listof (Pairof (U Natural #f) Natural))
                Natural
                (-> Float (-> Indexes Float)) Array))
(define (sequence n pattern tempo function)
  (: samples-per-beat Natural)
  (define samples-per-beat (quotient (* fs 60) tempo))
  (array-append*
   (for*/list : (Listof Array) ([i   (in-range n)] ; repeat the whole pattern
                                        [note : (Pairof (U Natural #f) Natural) (in-list  pattern)])
     (: nsamples Natural)
     (define nsamples (* samples-per-beat (cdr note)))
     (synthesize-note (car note) nsamples function))))
