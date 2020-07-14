#lang racket/base

(provide tzid-from-env)

(define (tzid-from-env)
  (getenv "TZ"))
