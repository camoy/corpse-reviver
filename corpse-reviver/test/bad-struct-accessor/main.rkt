#lang racket/base

(require "server.rkt")

#;(define (probably-ok x)
  (if (< x 0.001)
      (posn-x #f)
      (posn-x (posn 0 0))))

#;(probably-ok (random))
(posn-x #f)
