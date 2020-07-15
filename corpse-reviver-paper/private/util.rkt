#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide th
         sc
         scv-cr
         format-benchmark
         format-overhead
         format-percent
         format-interval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require scribble/base
         scribble/core
         scribble/html-properties
         scribble/latex-properties
         racket/runtime-path
         racket/format
         racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; styles

(define-runtime-path ROOT "..")
(define CSS (build-path ROOT "static" "css"))
(define TEX (build-path ROOT "static" "tex"))

(define sc-style
  (make-style "CapsSmall"
              (list (make-css-addition (build-path CSS "small-caps.css"))
                    (make-tex-addition (build-path TEX "small-caps.tex")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public

;; Element
(define th (superscript "th"))

;; Pre-Content ... → Element
;; Render the given content in small caps.
(define (sc . args)
  (apply elem #:style sc-style args))

(define scv-cr (sc "scv-cr"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formatters

(define (format-benchmark x)
  (sc (symbol->string x)))

;;
;; TODO
(define (format-overhead n)
  (string-append (~r n #:precision 1) "×"))

;;
;; TODO
(define (format-percent n)
  (string-append (~r (* 100 n) #:precision 0) "%"))

;;
;; TODO
(define (format-interval  μ+σ)
  (match-define (cons μ σ) μ+σ)
  (format "~a ± ~a" (ms-approx μ) (ms-approx σ)))

;;
;; TODO
(define (ms-approx x)
  (~r (/ x 1000) #:precision 0))
