#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide post-installer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require scribble/render
         scribble/pdf-render)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define MAIN "corpse-reviver.scrbl")

(define (post-installer _ collect-dir)
  (define doc (dynamic-require (build-path collect-dir MAIN) 'doc))
  (render (list doc)
          (list "main")
          #:render-mixin xelatex-render-mixin))
