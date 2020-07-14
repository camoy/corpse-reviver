#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide sc
         scv-cr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require scribble/base
         scribble/core
         scribble/html-properties
         scribble/latex-properties
         racket/runtime-path)

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

;; Pre-Content ... → Element
;; Render the given content in small caps.
(define (sc . args)
  (apply elem #:style sc-style args))

(define scv-cr (sc "scv-cr"))
