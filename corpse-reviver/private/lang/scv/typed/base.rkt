#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (all-from-out typed/racket/base/no-check)
         (all-from-out soft-contract/fake-contract)
         (all-from-out "core.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/require
         (subtract-in typed/racket/base/no-check
                      soft-contract/fake-contract
                      "core.rkt")
         (except-in soft-contract/fake-contract provide)
         "core.rkt")