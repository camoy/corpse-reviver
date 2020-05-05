#lang typed/racket/base

(provide
  log2
  natural->bitstring
  bitstring->natural
  in-reach
)

;; -----------------------------------------------------------------------------

(require
 (only-in racket/math exact-ceiling exact-truncate)
 scv-cr/opaque
 (only-in racket/list remove-duplicates))

(require/typed/opaque "fake-format.rkt"
  [~r (-> Exact-Rational
          Positive-Integer
          Nonnegative-Integer
          String
          String)])

;; =============================================================================

;; log, base 2
(: log2 (-> Index Index))
(define (log2 n)
  (define res (exact-ceiling (/ (log n) (log 2))))
  (if (index? res) res (error 'log2)))

;; Convert a natural number to a binary string, padded to the supplied width
(: natural->bitstring (-> Index Index String))
(define (natural->bitstring n pad-width)
  (~r n 2 pad-width "0"))

;; Convert a binary string to a natural number
(: bitstring->natural (-> String Index))
(define (bitstring->natural str)
  (define N (string-length str))
  (define res (for/sum : Integer ([i (in-range N)])
    (define c (string-ref str (- N (add1 i))))
    (if (equal? #\1 c)
        (exact-ceiling (expt 2 i))
        0)))
  (if (index? res) res (error 'bitstring->natural)))

;; Return a copy of `str` where the `i`-th bit is flipped.
;; (Flipped => 0 goes to 1 and 1 goes to 0)
(: bitstring-flip (-> String Index String))
(define (bitstring-flip str i)
  (define new (if (equal? #\0 (string-ref str i)) "1" "0"))
  (string-append (substring str 0 i)
                 new
                 (substring str (add1 i) (string-length str))))

;; Return all bitstrings reachable from `str`
;;  after incrementing at most `L` bits.
;; Result does NOT include the argument bitstring.
(: in-reach (-> String Index (Listof String)))
(define (in-reach str L)
  (cond [(zero? L) '()]
        [else
         (: res* (Listof (Listof String)))
         (define res*
           (for/list ([i (in-range (string-length str))]
                      #:when (equal? #\0 (string-ref str i)))
             (define str+ (bitstring-flip str (cast i Index)))
             (cons str+ (in-reach str+ (cast (sub1 L) Index)))))
         (remove-duplicates (apply append res*) string=?)]))
