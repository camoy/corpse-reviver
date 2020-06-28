#lang info

;; General

(define name "corpse-reviver-paper")
(define collection "corpse-reviver")
(define version "0.0")
(define scribblings
  '(("corpse-reviver.scrbl" (multi-page no-search))))

#;(define post-install-collection "private/render.rkt")

(define deps
  '("base"
    "gtp-plot"
    "pict-abbrevs"
    "scribble-lib"
    "threading-lib"))

(define build-deps
  '("chk-lib"
    "racket-doc"))
