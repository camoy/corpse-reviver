#lang typed/racket/base

(require corpse-reviver/require-typed-check
 (only-in racket/file file->lines file->string))

(require/typed "lcs.rkt"
  [longest-common-substring (-> String String String)])

(define LARGE_TEST "../base/prufock.txt")
(define SMALL_TEST "../base/hunt.txt")
(define KCFA_TYPED "../base/kcfa-typed.rkt")

;; LCS on all pairs of lines in a file
(: main (-> String Void))
(define (main testfile)
  (define lines (file->lines testfile))
  (time
    (for* ([a lines] [b lines])
      (longest-common-substring a b)))
  (void))

(main LARGE_TEST)
