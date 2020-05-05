#lang typed/racket/base

(require
  scv-cr/require-typed-check
  "summary-adapted.rkt")

(require/typed/check "spreadsheet.rkt"
                     [rktd->spreadsheet (-> Path-String Path-String Symbol Void)])

(require/typed/check "lnm-plot.rkt"
 [lnm-plot (-> Summary
               (Listof Index)
               Index
               Index
               Index
               Positive-Integer
               Float
               Positive-Integer
               Positive-Integer
               (Listof Any))])

;; Just testing

(: l-list (Listof Index))
(define l-list '(0 1 2 3))
(define NUM_SAMPLES 100)

(: main (-> String Void))
(define (main filename)
  ;; Parse data from input file (also creates module graph)
  (define summary (from-rktd filename))
  (define name (get-project-name summary))
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
