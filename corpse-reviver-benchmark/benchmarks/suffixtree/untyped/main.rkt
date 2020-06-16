#lang racket/base

(require (only-in racket/file file->lines file->string))

(require "lcs.rkt")

(define LARGE_TEST "../base/prufock.txt")
(define SMALL_TEST "../base/hunt.txt")
(define KCFA_TYPED "../base/kcfa-typed.rkt")

;; LCS on all pairs of lines in a file
(define (main testfile)
  (define lines (file->lines testfile))
  (time
    (for* ([a lines] [b lines])
      (longest-common-substring a b)))
  (void))

(main LARGE_TEST)
