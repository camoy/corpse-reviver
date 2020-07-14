#lang info

;; General

(define collection "corpse-reviver-paper")
(define version "0.0")
(define scribblings
  '(("scribblings/corpse-reviver-paper.scrbl" (multi-page no-search))))

#;(define post-install-collection "private/render.rkt")

(define deps '("base"))
(define build-deps
  '("chk-lib"
    "racket-doc"
    "gtp-plot"
    "pict-abbrevs"
    "mischief"
    "threading-lib"))
