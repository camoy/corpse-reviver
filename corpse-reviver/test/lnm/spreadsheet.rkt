#lang typed/racket/base

;; Convert a dataset to a spreadsheet.

;; Printed spreadsheets have:
;; - A title row, counting the number of runs for each variation
;; - A data row for each variation.
;;   The leftmost column is the configuration's bitstring, the rest of the
;;   columns are experimental results.

(provide
  ;; Convert a data file containing a vector to a spreadsheet
  ;; Vector must follow the format specified in the `data/` directory
  ;; (->* (Path-String) (#:output (U Path-String #f) #:format Symbol) String)
  rktd->spreadsheet
)
;; ----------------------------------------------------------------------------

(require
  corpse-reviver/require-typed-check
  (only-in racket/file file->value)
)
(require/typed/check "bitstring.rkt"
  [log2 (-> Index Index)]
  [natural->bitstring (-> Index Index String)])

;; =============================================================================

(define-syntax-rule (spreadsheet-error sym)
  (error 'from-rktd (format "Unknown spreadsheet format '~a'" sym)))

;; Convert a symbol representing a spreadsheet format into a file
;; extension for the spreadsheet.
(: symbol->extension (-> Symbol String))
(define (symbol->extension sym)
  (case sym
    [(csv tab) (string-append "." (symbol->string sym))]
    [else (spreadsheet-error sym)]))

;; Convert a symbol to a spreadsheet column separator.
(: symbol->separator (-> Symbol String))
(define (symbol->separator sym)
  (case sym
    [(csv) ","]
    [(tab) "\t"]
    [else (spreadsheet-error sym)]))

;; -----------------------------------------------------------------------------

;; (vector->spreadsheet rktd-vector out-file sep)
;; Copy the data from `rktd-vector` to the file `out-file`.
;; Format the data to a human-readable spreadsheet using `sep` to separate rows
(: vector->spreadsheet (-> (Vectorof (Listof Index)) Path-String String Void))
(define (vector->spreadsheet vec out-file sep)
  ;; First print the index
  (: num-configs Index)
  (define num-configs (vector-length vec))
  (define num-runs (length (vector-ref vec 0)))
  (void "Run")
  (for ([n num-runs])
    (void "~a~a" sep (add1 n)))
  (void)
  ;; For each row, print the config ID and all the values
  (for ([(row n) (in-indexed vec)])
    (void (natural->bitstring (cast n Index) (log2 num-configs)))
    (for ([v row]) (void "~a~a" sep v))
    (void)))

;; Print the rktd data stored in file `input-filename` to a spreadsheet.
(: rktd->spreadsheet (->* (Path-String) ((U Path-String #f) Symbol) Void))
(define (rktd->spreadsheet input-filename
                           [output #f]
                           [format 'tab])
  (define vec (cast (file->value input-filename) (Vectorof (Listof Index))))
  (define suffix (symbol->extension format))
  (define out (or output (path-replace-extension input-filename suffix)))
  (define sep (symbol->separator format))
  (vector->spreadsheet vec out sep))
