#lang typed/racket/base

;; Data structure representing the results of one experiment.
;; Handles queries for raw data and statistical summary data.

(provide
  all-variations
  from-rktd
  get-num-variations
  get-project-name
  predicate->variations
  (struct-out summary)
  untyped-mean
  variation->mean-runtime
)

;; -----------------------------------------------------------------------------

(require
  corpse-reviver/require-typed-check
  racket/path
  (only-in racket/file file->value)
  (only-in racket/vector vector-append)
  "modulegraph-adapted.rkt")

(require (only-in math/statistics mean))

(require/typed racket/stream
  [stream-map (-> (-> Index String) (Sequenceof Index) (Sequenceof String))]
  [stream-filter (-> (-> String Boolean) (Sequenceof String) (Sequenceof String))]
)
(require/typed/check "bitstring.rkt"
  [bitstring->natural (-> String Index)]
  [log2 (-> Index Index)]
  [natural->bitstring (-> Index Index String)])

;; =============================================================================
;; -- data definition: summary

(struct summary (
  [source : Path-String] ;; the data's origin
  [dataset : (Vectorof (Listof Index))] ;; the underlying experimental data
  [modulegraph : ModuleGraph] ;; the adjacency list of the represented project
))
(define-type Summary summary)

;; -----------------------------------------------------------------------------
;; -- constants

;; Default location for TiKZ module graphs
(define MODULE_GRAPH_DIR "module-graphs")

;; -----------------------------------------------------------------------------
;; -- parsing

(define-syntax-rule (parse-error msg arg* ...)
  (error 'summary (format msg arg* ...)))

;; Create a summary from a raw dataset.
;; Infers the location of the module graph if #:graph is not given explicitly
(: from-rktd (->* [String] [(U Path #f)] Summary))
(define (from-rktd filename [graph-path #f])
  (define path (string->path filename))
  (define dataset (rktd->dataset path))
  (define mg (from-tex (or graph-path (infer-graph path))))
  (validate-modulegraph dataset mg)
  (summary path dataset mg))

;; Parse a dataset from a filepath.
(: rktd->dataset (-> Path (Vectorof (Listof Index))))
(define (rktd->dataset path)
  ;; Check .rktd
  (unless (bytes=? (string->bytes/utf-8 "rktd") (or (filename-extension path) (string->bytes/utf-8 "")))
    (parse-error "Cannot parse dataset '~a', is not .rktd" (path->string path)))
  ;; Get data
  (define vec (file->value path))
  ;; Check invariants
  (validate-dataset vec))

;; Confirm that the dataset `vec` is a well-formed vector of experiment results.
(: validate-dataset (-> Any (Vectorof (Listof Index))))
(define (validate-dataset vec0)
  (define vec (cast vec0 (Vectorof (Listof Index))))
  (unless (< 0 (vector-length vec)) (parse-error "Dataset is an empty vector, does not contain any entries"))
  ;; Record the number of runs in the first vector, match against other lengths
  (: num-runs (Boxof (U #f Index)))
  (define num-runs (box #f))
  (for ([row-index (in-range (vector-length vec))])
    (define inner (vector-ref vec row-index))
    (unless (list? inner) (parse-error "Dataset is not a vector of lists found non-list entry '~a'" inner))
    ;; unboxed is for occurrence typing
    (define unboxed (unbox num-runs))
    (if (not unboxed)
        (set-box! num-runs (length inner))
        (unless (= unboxed (length inner)) (parse-error "Rows 0 and ~a of dataset have different lengths; all variations must describe the same number of runs.\n  Bad row: ~a" row-index inner)))
    (for ([val (in-list inner)])
      (unless (exact-positive-integer? val)
        (parse-error "Row ~a contains non-integer entry '~a'" row-index val))))
    vec)

;; Check that the dataset and module graph agree
(: validate-modulegraph (-> (Vectorof (Listof Index)) ModuleGraph Void))
(define (validate-modulegraph dataset mg)
  (define ds-num-modules (log2 (vector-length dataset)))
  (define mg-num-modules (length (module-names mg)))
  (unless (= ds-num-modules mg-num-modules)
    (parse-error "Dataset and module graph represent different numbers of modules. The dataset says '~a' but the module graph says '~a'" ds-num-modules mg-num-modules)))

;; Guess the location of the module graph matching the dataset
(: infer-graph (-> Path Path-String))
(define (infer-graph path)
  ;; Get the prefix of the path
  (define tag (path->project-name path))
  ;; Search in the MODULE_GRAPH_DIR directory for a matching TeX file
  (define relative-pathstring (format "../~a/~a.tex" MODULE_GRAPH_DIR tag))
  (build-path (or (path-only path) (error 'infer-graph))
              (string->path relative-pathstring)))

;; -----------------------------------------------------------------------------
;; -- querying

(: all-variations (-> Summary (Sequenceof String)))
(define (all-variations sm)
  (define M (get-num-modules sm))
  (stream-map (lambda ([n : Index]) (natural->bitstring n M))
              (in-range (get-num-variations sm))))

(: get-module-names (-> Summary (Listof String)))
(define (get-module-names sm)
  (module-names (summary-modulegraph sm)))

(: get-num-variations (-> Summary Index))
(define (get-num-variations sm)
  (vector-length (summary-dataset sm)))

(: get-num-modules (-> Summary Index))
(define (get-num-modules sm)
  (length (get-module-names sm)))

(: get-project-name (-> Summary String))
(define (get-project-name sm)
  (project-name (summary-modulegraph sm)))

(: predicate->variations (-> Summary (-> String Boolean) (Sequenceof String)))
(define (predicate->variations sm p)
  (stream-filter p (all-variations sm)))

;; Return all data for the untyped variation
(: untyped-runtimes (-> Summary (Listof Index)))
(define (untyped-runtimes sm)
  (vector-ref (summary-dataset sm) 0))

(: untyped-mean (-> Summary Real))
(define (untyped-mean sm)
  (mean (untyped-runtimes sm)))

;; Return all data for the typed variation
(: typed-runtimes (-> Summary (Listof Index)))
(define (typed-runtimes sm)
  (define vec (summary-dataset sm))
  (vector-ref vec (sub1 (vector-length vec))))

(: variation->mean-runtime (-> Summary String Real))
(define (variation->mean-runtime sm var)
  (index->mean-runtime sm (bitstring->natural var)))

(: index->mean-runtime (-> Summary Index Real))
(define (index->mean-runtime sm i)
  (mean (vector-ref (summary-dataset sm) i)))

;; ;; Return all data for all gradually-typed variations
;; (: gradual-runtimes (-> Summary (Listof String)))
;; (define (gradual-runtimes sm)
;;   (define vec (summary-dataset sm))
;;   ;; Efficient enough?
;;   (apply append
;;          (for/list ([i (in-range 1 (sub1 (vector-length vec)))])
;;            (vector-ref vec i))))
