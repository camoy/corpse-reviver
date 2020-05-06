#lang typed/racket/base

;; Copyright 2014 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0


;; -----------------------------------------------------------------------------

(require
  corpse-reviver/require-typed-check
  (only-in racket/file file->value))

(require/typed/check "morse-code-strings.rkt"
  [string->morse (-> String String)])

(require/typed/check "levenshtein.rkt"
               [string-levenshtein (String String -> Integer)])

(define word-frequency-list "./../base/frequency.rktd")
(define word-frequency-list-small "./../base/frequency-small.rktd")

(define-predicate freq-list? (Listof (List String Integer)))

(: file->words (-> String (Listof String)))
(define (file->words filename)
  (define words+freqs (file->value (string->path filename)))
  (unless (freq-list? words+freqs) (error "expected a frequency list"))
  (for/list : (Listof String) ([word+freq : (List String Integer) words+freqs])
    (car word+freq)))

(: allwords (Listof String))
(define allwords (file->words word-frequency-list))

(: words-small (Listof String))
(define words-small (file->words word-frequency-list-small))

(: main (-> (Listof String) Void))
(define (main words)
  (for* ([w1 (in-list words)]
         [w2 (in-list words)])
    (string->morse w1)
    (string->morse w2)
    (string-levenshtein w1 w2)
    (string-levenshtein w2 w1)
    (void)))

(time (main words-small))
