#lang racket/base

;; Provides functions for drawing lattices that show the perf data
;; for each configuration

(require racket/contract
         racket/format
         racket/list
         data/bit-vector
         racket/vector
         (except-in math/number-theory permutations)
         (only-in racket/file file->value)
         (only-in math/statistics mean stddev)
         pict)

(provide
  (contract-out
    [file->performance-lattice
     (-> path-string? pict?)]
    [make-performance-lattice
     (->* ((and/c (vectorof (cons/c number? number?))
                  power-of-two-length?)
           (and/c (vectorof (cons/c number? number?))
                  power-of-two-length?))
          (#:just-one (one-of/c #f 'scv 'baseline))
         pict?)]))

(define-syntax-rule (define/provide nm x* ...)
  (begin (define nm x* ...) (provide nm)))

(define/provide *LATTICE-BORDER-WIDTH* (make-parameter 2))
(define/provide *LATTICE-BOX-BOT-MARGIN* (make-parameter 8))
(define/provide *LATTICE-BOX-HEIGHT* (make-parameter 26))
(define/provide *LATTICE-BOX-SEP* (make-parameter 3))
(define/provide *LATTICE-BOX-TOP-MARGIN* (make-parameter 4))
(define/provide *LATTICE-BOX-WIDTH* (make-parameter 16))
(define/provide *LATTICE-CONFIG-MARGIN* (make-parameter 10))
(define/provide *LATTICE-LEVEL-MARGIN* (make-parameter 20))
(define/provide *LATTICE-FONT-SIZE* (make-parameter 16))
(define/provide *LATTICE-TRUNCATE-DECIMALS?* (make-parameter #f))
(define/provide *LATTICE-LINE-WIDTH* (make-parameter 1))
(define/provide *LATTICE-LINE-ALPHA* (make-parameter 0.4))
(define/provide *LATTICE-RED-THRESHOLD* (make-parameter 3))
(define/provide *LATTICE-GREEN-THRESHOLD* (make-parameter 1.25))
(define/provide *LATTICE-RED-COLOR* (make-parameter "Pink"))
(define/provide *LATTICE-GREEN-COLOR* (make-parameter "Pale Green"))

(module+ test (require rackunit))

(define (power-of-two-length? vec)
  (define factors (factorize (vector-length vec)))
  (and (= 1 (length factors))
       (= (car (car factors)) 2)))

(define (two-expt n) (cadar (factorize n)))

(module+ test
  (check-true (power-of-two-length? (vector 1 2 3 4)))
  (check-false (power-of-two-length? (vector 1 2 3))))

(define (make-performance-lattice baseline-vec data-vec #:just-one [jo #f])
  (define total-bits (two-expt (vector-length data-vec)))
  (define pict-vec (make-vector (vector-length data-vec) #f))
  (define level-picts
    (for/list ([on-bits (in-range total-bits -1 -1)])
      (define perms (select (- total-bits on-bits) total-bits))
      (apply hc-append (*LATTICE-CONFIG-MARGIN*)
       (for/list ([perm (in-list perms)])
         (define bv (apply bit-vector perm))
         (define num (string->number (bit-vector->string bv) 2))
         (define pict (make-point bv
                                  (vector-ref data-vec num)
                                  (vector-ref data-vec 0)
                                  (vector-ref baseline-vec num)
                                  (vector-ref baseline-vec 0)
                                  #:just-one jo))
         (vector-set! pict-vec num pict)
         pict))))
  (define no-lines-yet (apply vc-append (*LATTICE-LEVEL-MARGIN*) level-picts))
  (add-all-lines no-lines-yet pict-vec total-bits))

;; taken from MF's version
(define (select i L)
  (cond
    [(zero? i)  (list (build-list L (lambda (_) #t)))]
    [(zero? L) '()]
    [else (append (map (lambda (r) (cons #f r)) (select (sub1 i) (sub1 L)))
                  (map (lambda (r) (cons #t r)) (select i (sub1 L))))]))

(define (with-bg pict color [margin 1])
  (define-values (w h)
    (values (+ (pict-width pict) (* 2 margin))
            (+ (pict-height pict) (* 2 margin))))
  (cc-superimpose
   (filled-rounded-rectangle w h #:color color #:draw-border? #f)
   pict))

(define (mean->string x)
  (~a
   (if (and (*LATTICE-TRUNCATE-DECIMALS?*) (<= 1 x))
       (number->string (round x))
       (~r x #:precision 1))
   "×"))

(define (subtext scv baseline #:just-one [jo #f])
  (define-values (scv-str baseline-str)
    (values (mean->string scv)
            (mean->string baseline)))
  (define style "Liberation Serif")
  (define (color x)
    (cond
      [(<= x (*LATTICE-GREEN-THRESHOLD*)) (*LATTICE-GREEN-COLOR*)]
      [(>= x (*LATTICE-RED-THRESHOLD*)) (*LATTICE-RED-COLOR*)]
      [else "White"]))
  (define-values (scv-pict baseline-pict)
    (values (with-bg
              (text scv-str null (*LATTICE-FONT-SIZE*))
              (color scv))
            (with-bg
              (text baseline-str null (*LATTICE-FONT-SIZE*))
              (color baseline))))
  (cond
    [(eq? jo 'scv) scv-pict]
    [(eq? jo 'baseline) baseline-pict]
    [else (hc-append baseline-pict
                     (text " " null (*LATTICE-FONT-SIZE*))
                     scv-pict)]))

;; constructs a pict for a point in the lattice, using the initial
;; configuration to normalize (for coloring)
(define (make-point bv data init-data baseline-data baseline-init-data
                    #:just-one [jo #f])
  (define normalized-mean (/ (car data) (car init-data)))
  (define normalized-baseline-mean (/ (car baseline-data)
                                      (car baseline-init-data)))
  (define normalized-stdev (/ (cdr data) (car init-data)))
  (define box-pict
    (apply hc-append
           (*LATTICE-BOX-SEP*)
           (for/list ([bit (in-bit-vector bv)])
             (filled-rectangle (*LATTICE-BOX-WIDTH*) (*LATTICE-BOX-HEIGHT*)
                             #:color (if bit "black" "white")
                             #:border-width (*LATTICE-BORDER-WIDTH*)
                             #:border-color "black"))))
  (vc-append (blank 1 (*LATTICE-BOX-TOP-MARGIN*))
             box-pict
             (blank 1 (*LATTICE-BOX-BOT-MARGIN*))
             (subtext normalized-mean normalized-baseline-mean
                      #:just-one jo)))

;; adds lines between elements in levels
(define (add-all-lines base vec bits)
  (define line-width (*LATTICE-LINE-WIDTH*))
  (define line-alpha (*LATTICE-LINE-ALPHA*))
  (for/fold ([pict base])
            ([(from-pict idx) (in-indexed (in-vector vec))])
    (define masks
      (for/list ([bools (in-list (select (- bits 1) bits))])
        (string->number (apply string-append (map (λ (x) (if x "1" "0")) bools))
                        2)))
    (define targets
      (remove* (list idx)
               (map (λ (x) (bitwise-ior x idx)) masks)))
    (for/fold ([pict pict])
              ([target-idx targets])
      (define target (vector-ref vec target-idx))
      (pin-line pict from-pict ct-find target cb-find
                      #:alpha line-alpha
                      #:line-width line-width
                      #:under? #t))))

;; Driver submodule for rapid visualization
(module+ main
  (require racket/gui/base
           racket/match)
  (match (current-command-line-arguments)
    [(vector path)
     (show-pict (file->performance-lattice path))]))

(define (file->performance-lattice path)
  (define data (file->value path))
  (define averaged-results
    (vector-map (λ (times) (cons (mean times) (stddev times))) data))
  (make-performance-lattice averaged-results))
