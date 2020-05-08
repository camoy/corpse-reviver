#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide debug
         measure
         scv-cr-logger)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/pretty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; logger

(define-logger scv-cr)

;; String Any ... → Void
;; Log value for debugging purposes (and make it pretty).
(define-syntax-rule (debug fmt x ...)
  (log-scv-cr-debug fmt
                    (if (syntax? x)
                        (pretty-format (syntax->datum x))
                        x)
                    ...))

(define-syntax-rule (measure key tgt x ...)
  (let-values ([(results cpu-time real-time gc-time)
                (time-apply (λ () (begin x ...)) '())])
    (log-scv-cr-info "~s" (list key tgt cpu-time real-time gc-time))
    (apply values results)))
