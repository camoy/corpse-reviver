#lang racket/base

(provide ~dvh:cite dvh:citet dvh:generate-bibliography
         ~sth:cite sth:citet sth:generate-bibliography
         ~local:cite local:citet local:generate-bibliography)

(require racket/runtime-path
         scriblib/bibtex)

(define-runtime-path CWD ".")
(define BIB (build-path CWD ".." "bib"))
(define DVH (build-path BIB "dvh-bibliography.bib"))
(define STH (build-path BIB "sth-bibliography.bib"))
(define LOCAL (build-path BIB "local.bib"))

(define-bibtex-cite DVH ~dvh:cite dvh:citet dvh:generate-bibliography)
(define-bibtex-cite STH ~sth:cite sth:citet sth:generate-bibliography)
(define-bibtex-cite LOCAL ~local:cite local:citet local:generate-bibliography)
