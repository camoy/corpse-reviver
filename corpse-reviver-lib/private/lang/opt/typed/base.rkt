#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (all-from-out typed/racket/base)
         (all-from-out "core.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (except-in typed/racket/base
                    require/typed
                    require/typed/provide
                    provide
                    ;; HACK: For kCFA & zordoz benchmarks (see TR bug 931).
                    exp)
         "core.rkt")