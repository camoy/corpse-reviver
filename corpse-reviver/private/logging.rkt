#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide debug
         info
         measure
         warn
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

(define-syntax-rule (warn str)
  (log-scv-cr-warning "~s" (list 'warning #f str)))

(define-syntax-rule (info key datum)
  (log-scv-cr-info "~s" (list key #f datum)))

(define-syntax-rule (measure key tgt x ...)
  (let-values ([(results cpu-time real-time gc-time)
                (time-apply (λ () (begin x ...)) '())])
    (log-scv-cr-info "~s" (list key tgt (list cpu-time real-time gc-time)))
    (apply values results)))
