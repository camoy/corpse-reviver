#lang racket/base

(require racket/contract/base
         racket/match
         racket/vector)
(require "structs.rkt")

(provide/contract
 [parse-tzfile (-> path-string? string? (vector/c (vectorof interval?) (vectorof tzoffset?)))])

(struct header
  (magic version gmtcnt stdcnt leapcnt txcnt offcnt chrcnt)
  #:transparent)

(struct offset
  (utc-seconds
   dst?
   abbreviation-index)
  #:transparent)

(define (parse-tzfile dir tzid)
  (define path (build-path dir tzid))
  (unless (file-exists? path)
    (raise (exn:fail:tzinfo:zone-not-found
            (format "Cannot find zoneinfo file for [~a]" tzid)
            (current-continuation-marks))))
  (call-with-input-file* path parse-tzfile-contents))

(define (skip-32bit-section in header32)
  (match header32
    [(header _ _ g s l t o c)
     (skip-bytes in (+ (* 5 t) (* 6 o) (* 8 l) g s c))]))

(define (parse-transition-times in header word-size)
  (for/list ([i (in-range (header-txcnt header))])
    (integer-bytes->integer (read-bytes word-size in) #t #t)))

(define (parse-transition-offsets in header)
  (for/list ([i (in-range (header-txcnt header))])
    (read-byte in)))

(define (parse-offsets in header)
  (for/vector ([i (in-range (header-offcnt header))])
    (offset (integer-bytes->integer (read-bytes 4 in) #t #t)
            (= (read-byte in) 1)
            (read-byte in))))

(define (build-tzoffsets offsets abbreviation-bytes)
  (vector-map (λ (o)
                (match o
                  [(offset t d? i)
                   (tzoffset t
                             d?
                             (string->immutable-string
                              (bytes->string/utf-8
                               (car (regexp-match #px#"[^\0]+" abbreviation-bytes i)))))]))
              offsets))

(define (build-intervals txtimes offset-indices tzoffsets)
  (define base-intervals
    (reverse
     (let loop ([res '()] [xs txtimes] [ys offset-indices])
       (match* (xs ys)
         [('() '())
          res]
         [((list t) (list i))
          (cons (interval t +inf.0 (vector-ref tzoffsets i))
                res)]
         [((list-rest t1 t2 xs) (cons i ys))
          (loop
           (cons (interval t1 t2 (vector-ref tzoffsets i))
                 res)
           (cons t2 xs)
           ys)]))))
  
  (match base-intervals
    ['()
     ;; no intervals: create a single, infinite interval
     (list (interval -inf.0 +inf.0 (vector-ref tzoffsets 0)))]
    [(cons (interval _ t (and off (tzoffset _ #f _))) xs)
     ;; our first interval is in standard time: extend it to -inf.0
     (cons (interval -inf.0 t off) xs)]
    [(cons (and x (interval t _ _)) xs)
     ;; our first interval is in DST: prepend a starting interval
     (list* (interval -inf.0 t (first-standard-offset base-intervals))
            x
            xs)]))

(define (first-standard-offset intervals)
  (for*/first ([int (in-list intervals)]
               [off (in-value (interval-offset int))]
               #:unless (tzoffset-dst? off))
    off))
    
      

(define (parse-tzfile-contents in)
  (define header32 (parse-header in))
  (match-define (vector header word-size)
    (match (header-version header32)
      [(or #"2" #"3")
       (skip-32bit-section in header32)
       (vector (parse-header in) 8)]
      [_
       (vector header32 4)]))
  
  (define transition-times    (parse-transition-times in header word-size))
  (define transition-offsets  (parse-transition-offsets in header))
  (define offsets             (parse-offsets in header))
  (define abbreviation-bytes  (read-bytes (header-chrcnt header) in))
  (define tzoffsets           (build-tzoffsets offsets abbreviation-bytes))
  (define intervals           (build-intervals transition-times transition-offsets tzoffsets))
  
  
  (vector (list->vector intervals)
          tzoffsets))
  

(define (parse-header in)
  (define bs (read-bytes 44 in))
  (header (subbytes bs 0 4)
          (subbytes bs 4 5)
          (integer-bytes->integer (subbytes bs 20 24) #f #t)
          (integer-bytes->integer (subbytes bs 24 28) #f #t)
          (integer-bytes->integer (subbytes bs 28 32) #f #t)
          (integer-bytes->integer (subbytes bs 32 36) #f #t)
          (integer-bytes->integer (subbytes bs 36 40) #f #t)
          (integer-bytes->integer (subbytes bs 40 44) #f #t)))

          
(define (skip-bytes in n)
  (file-position in (+ n (file-position in))))
