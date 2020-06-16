#lang racket/base

;; Convert a dataset to a spreadsheet.

;; Printed spreadsheets have:
;; - A title row, counting the number of runs for each variation
;; - A data row for each variation.
;;   The leftmost column is the configuration's bitstring, the rest of the
;;   columns are experimental results.

(provide
  rktd->spreadsheet
)
;; ----------------------------------------------------------------------------

(require
  "../base/untyped.rkt"
  (only-in racket/file file->value)
  (only-in "bitstring.rkt" log2 natural->bitstring)
)

;; =============================================================================

(define-syntax-rule (spreadsheet-error sym)
  (error 'from-rktd (format "Unknown spreadsheet format '~a'" sym)))

;; Convert a symbol representing a spreadsheet format into a file
;; extension for the spreadsheet.
;; (: symbol->extension (-> Symbol String))
(define (symbol->extension sym)
  (case sym
    [(csv tab) (string-append "." (symbol->string sym))]
    [else (spreadsheet-error sym)]))

;; Convert a symbol to a spreadsheet column separator.
;; (: symbol->separator (-> Symbol String))
(define (symbol->separator sym)
  (case sym
    [(csv) ","]
    [(tab) "\t"]
    [else (spreadsheet-error sym)]))

;; -----------------------------------------------------------------------------

;; (vector->spreadsheet rktd-vector out-file sep)
;; Copy the data from `rktd-vector` to the file `out-file`.
;; Format the data to a human-readable spreadsheet using `sep` to separate rows
;; (: vector->spreadsheet (-> (Vectorof (Listof Index)) Path-String String Void))
(define (vector->spreadsheet vec out-file sep)
  ;; First print the index
  (define num-configs (vector-length vec))
  (define num-runs (length (vector-ref vec 0)))
  (void "Run")
  (for ([n num-runs])
    (void "~a~a" sep (add1 n)))
  (void)
  ;; For each row, print the config ID and all the values
  (for ([(row n) (in-indexed vec)])
    (void (natural->bitstring (assert n index?) (log2 num-configs)))
    (for ([v row]) (void "~a~a" sep v))
    (void)))

;; Print the rktd data stored in file `input-filename` to a spreadsheet.
;; (: rktd->spreadsheet (->* (Path-String) (#:output (U Path-String #f) #:format Symbol) String))
(define (rktd->spreadsheet input-filename
                           [output #f]
                           [format 'tab])
  (define vec
    (for/vector ((x (in-vector (assert (file->value input-filename) vector?))))
      (listof-index x)))
  (define suffix (symbol->extension format))
  (define out (or output (path-replace-suffix input-filename suffix)))
  (define sep (symbol->separator format))
  (vector->spreadsheet vec out sep))

(define (listof-index x)
  (if (and (list? x)
           (andmap index? x))
    x
    (error 'listof-index)))
