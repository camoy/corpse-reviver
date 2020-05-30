#lang info

;; General

(define collection 'multi)
(define version "0.0")
(define pkg-desc "Sound and efficient gradual typing via contract verification.")
(define pkg-authors '(camoy))

;; Dependencies

(define deps
  '("base"
    "corpse-reviver-benchmark/"
    "corpse-reviver-lib/"
    "corpse-reviver-doc/"))

(define implies
  '("corpse-reviver-benchmark"
    "corpse-reviver-lib"
    "corpse-reviver-doc"))

(define build-deps '())
