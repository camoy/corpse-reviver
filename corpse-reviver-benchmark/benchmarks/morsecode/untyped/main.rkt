#lang racket/base

;; Copyright 2014 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0


;; -----------------------------------------------------------------------------

(require
  corpse-reviver/require-typed-check
  (only-in racket/file file->value))

(require (only-in "morse-code-strings.rkt"
  string->morse))

(require (only-in "levenshtein.rkt"
               string-levenshtein))

(define word-frequency-list "./../base/frequency.rktd")
(define word-frequency-list-small "./../base/frequency-small.rktd")

(define (freq-list? x)
  (and (list? x)
       (andmap list-string-int x)))

(define (list-string-int x)
  (and (pair? x)
       (pair? (cdr x))
       (null? (cdr (cdr x)))
       (string? (car x))
       (exact-integer? (car (cdr x)))))

(define (file->words filename)
  (define words+freqs (file->value (string->path filename)))
  (unless (freq-list? words+freqs) (error "expected a frequency list"))
  (for/list ([word+freq  words+freqs])
    (car word+freq)))

(define allwords (file->words word-frequency-list))

(define words-small (file->words word-frequency-list-small))

(define (main words)
  (for* ([w1 (in-list words)]
         [w2 (in-list words)])
    (string->morse w1)
    (string->morse w2)
    (string-levenshtein w1 w2)
    (string-levenshtein w2 w1)
    (void)))

(time (main words-small))
