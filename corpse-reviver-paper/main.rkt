#lang racket/base

(provide
 (prefix-out figure: (all-from-out "private/figure.rkt"))
 (prefix-out stat: (all-from-out "private/stat.rkt"))
 (all-from-out
  scribble/acmart
  scribble/acmart/lang
  scriblib/figure
  "private/cite.rkt")
 CORPSE-REVIVER-PAPER-STYLE)

(require racket/runtime-path
         scribble/acmart
         scribble/acmart/lang
         scribble/core
         scriblib/figure
         scribble/html-properties
         "private/cite.rkt"
         "private/figure.rkt"
         "private/stat.rkt")

(define-runtime-path CWD ".")
(define CSS (build-path CWD "static" "css"))

(define CORPSE-REVIVER-PAPER-STYLE
  (make-style "style"
              (list (make-css-addition (build-path CSS "style.css"))
                    (render-convertible-as '(svg-bytes png-bytes)))))
