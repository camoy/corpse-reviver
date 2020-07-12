#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide measure-gc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/logging)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct gc-info (major? pre-amount pre-admin-amount code-amount
                 post-amount post-admin-amount
                 start-process-time end-process-time
                 start-time end-time)
  #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; (→ Any) → Any [Hash Symbol Number]
;; Given a thunk, returns a hash containing GC information while executing the
;; thunk.
(define (measure-gc thunk)
  (define buf (box null))
  (define-values (initial-times)
    (cons (current-process-milliseconds)
          (current-inexact-milliseconds)))
  (define result
    (with-intercepted-logging
      (λ (v)
        (when (eq? 'gc-info (prefab-struct-key (vector-ref v 2)))
          (set-box! buf (cons v (unbox buf)))))
      thunk
      'debug
      #:logger (current-logger)))
  (define-values (final-times)
    (cons (current-process-milliseconds)
          (current-inexact-milliseconds)))
  (values result
          (summarize (unbox buf) initial-times final-times)))

;; List [Cons Number Number] [Cons Number Number] → Hash
;; Given a list of GC results and start and end times, constructs the summary
;; of that GC data as a hash.
(define (summarize results initial-times final-times)
  (let/cc return
    (define-values (end-proc-time end-time)
      (values (car final-times) (cdr final-times)))
    (define gc-results
      (for*/list ([e (in-list results)]
                  [v (in-value (vector-ref e 2))]
                  #:when (gc-info? v))
        v))

    (unless (pair? gc-results)
      (return (hash)))

    (define num-major (for/sum ([e gc-results] #:when (gc-info-major? e)) 1))
    (define num-minor (for/sum ([e gc-results] #:unless (gc-info-major? e)) 1))
    (define allocated (+
                       (gc-info-pre-amount (car gc-results))
                       (for/sum ([i (in-list gc-results)]
                                 [j (in-list (cdr gc-results))])
                         (- ;; total heap size here
                          (gc-info-pre-amount j)
                          ;; size of heap after last collection
                          (gc-info-post-amount i)))))
    (define collected (for/sum ([i (in-list gc-results)])
                        (- (gc-info-pre-amount i)
                           (gc-info-post-amount i))))
    (define max-heap-size
      (apply max (for/list ([i gc-results]) (gc-info-pre-amount i))))
    (define max-used
      (apply max (for/list ([i gc-results])
                   (+ (gc-info-pre-admin-amount i)
                      (gc-info-code-amount i)))))
    (define max-slop
      (apply max (for/list ([i gc-results])
                   (max
                    (- (gc-info-pre-admin-amount i)
                       (gc-info-pre-amount i))
                    (- (gc-info-post-admin-amount i)
                       (gc-info-post-amount i))))))
    (define startup-time (car initial-times))
    (define total-time (- end-proc-time startup-time))
    (define total-elapsed-time (- end-time (cdr initial-times)))
    (define minor-gc-time
      (for/sum ([i (in-list gc-results)] #:unless (gc-info-major? i))
        (- (gc-info-end-process-time i) (gc-info-start-process-time i))))
    (define minor-gc-elapsed-time
      (for/sum ([i (in-list gc-results)] #:unless (gc-info-major? i))
        (- (gc-info-end-time i) (gc-info-start-time i))))
    (define major-gc-time
      (for/sum ([i (in-list gc-results)] #:when (gc-info-major? i))
        (- (gc-info-end-process-time i) (gc-info-start-process-time i))))
    (define major-gc-elapsed-time
      (for/sum ([i (in-list gc-results)] #:when (gc-info-major? i))
        (- (gc-info-end-time i) (gc-info-start-time i))))
    (define max-gc-pause
      (for/fold ([mx 0]) ([i (in-list gc-results)])
        (max mx (- (gc-info-end-time i) (gc-info-start-time i)))))
    (define gc-time (+ minor-gc-time major-gc-time))
    (define gc-elapsed-time (+ minor-gc-elapsed-time major-gc-elapsed-time))
    (define mut-time (- total-time gc-time))
    (define mut-elapsed-time (- total-elapsed-time gc-elapsed-time))
    (define gc% (* 100. (/ gc-time total-time)))
    (define gc-elapsed% (* 100. (/ gc-elapsed-time total-elapsed-time)))
    (define alloc-rate (* 1000. #|time in ms|# (/ allocated mut-time)))
    (hash 'gc-alloc allocated
          'gc-collected collected
          'gc-max-heap-size max-heap-size
          'gc-max-slop max-slop
          'gc-peak-total-memory max-used
          'gc-num-minor num-minor
          'gc-minor-gc-time minor-gc-time
          'gc-minor-gc-elapsed-time minor-gc-elapsed-time
          'gc-num-major num-major
          'gc-major-gc-time major-gc-time
          'gc-major-gc-elapsed-time major-gc-elapsed-time
          'gc-max-pause max-gc-pause
          'gc-percent-time gc%
          'gc-percent-elapsed gc-elapsed%
          'gc-alloc-rate (inexact->exact (round alloc-rate)))))
