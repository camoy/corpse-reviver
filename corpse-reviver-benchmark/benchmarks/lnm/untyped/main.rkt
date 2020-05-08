#lang racket/base

(require
  (only-in "spreadsheet.rkt" rktd->spreadsheet)
  (only-in "summary.rkt" get-project-name from-rktd)
  (only-in "lnm-plot.rkt" lnm-plot)
)

;; Just testing

(define L-max 3)
(define NUM_SAMPLES 100)

(define (main filename)
  ;; Parse data from input file (also creates module graph)
  (define summary (from-rktd filename))
  (define name (get-project-name summary))
  (define l-list (for/list ([i (in-range (add1 L-max))]) i))
  ;; Create L-N/M pictures
  (time
    (begin
      (lnm-plot summary l-list
                        3
                        10
                        20
                        NUM_SAMPLES
                        0.6
                        400
                        300)
      (rktd->spreadsheet filename "./test-case-output.out" 'tab)
      (void))))

(main "../base/data/snake.rktd")
