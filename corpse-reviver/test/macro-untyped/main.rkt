#lang racket/base

(provide foo)

(require (for-syntax racket/base))

(define-syntax (foo stx)
  #'(void))
