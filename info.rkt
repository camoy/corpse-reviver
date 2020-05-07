#lang info

;; General information

(define collection 'multi)
(define version "0.0")
(define pkg-desc "Sound and efficient gradual typing via contract verification.")
(define pkg-authors '("Cameron Moy"))

;; Dependencies

(define deps
  '(("base" #:version "7.4")
    "fancy-app"
    "graph"
    "lang-file"
    "mischief"
    "git://github.com/camoy/soft-contract.git#scv-cr"
    "threading"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "rackunit-lib"
    "chk"))
