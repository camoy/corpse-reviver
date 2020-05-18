#lang racket/base

(require "server.rkt")

(define (actually-ok x)
  (adder 0))

(actually-ok (random))
