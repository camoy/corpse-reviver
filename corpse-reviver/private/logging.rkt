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

;; Logger
;; Logger for SCV-CR.
(define-logger scv-cr)

;; String Any ... → Void
;; Log value for debugging purposes (and make it pretty).
(define-syntax-rule (debug fmt x ...)
  (log-scv-cr-debug fmt
                    (if (syntax? x)
                        (pretty-format (syntax->datum x))
                        x)
                    ...))

;; String → Void
;; Log an SCV-CR warning.
(define-syntax-rule (warn str)
  (log-scv-cr-warning "~s" (list 'warning #f str)))

;; String → Void
;; Log an SCV-CR info item. This is annotated with a "key" to stratify
;; different kinds of info.
(define-syntax-rule (info key datum)
  (log-scv-cr-info "~s" (cons key datum)))

;; String Any ... → Void
;; Measures how long the given code takes to run and logs that as info. This is
;; monitored externally when benchmarking.
(define-syntax-rule (measure key x ...)
  (let-values ([(results cpu-time real-time gc-time)
                (time-apply (λ () (begin x ...)) '())])
    (log-scv-cr-info "~s" (cons key (list cpu-time real-time gc-time)))
    (apply values results)))
