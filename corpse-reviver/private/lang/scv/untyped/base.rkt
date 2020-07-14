#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (all-from-out racket/base)
         (all-from-out soft-contract/fake-contract)
         (all-from-out "core.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/require
         (subtract-in racket/base
                      soft-contract/fake-contract
                      "core.rkt")
         soft-contract/fake-contract
         "core.rkt")
