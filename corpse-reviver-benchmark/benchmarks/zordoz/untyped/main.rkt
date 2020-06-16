#lang racket/base

(require "zo-shell.rkt")

(define TYPED-DATA
  '("../base/typed-zo-find_rkt.zo"))

(define UNTYPED-DATA
  '("../base/main_rkt.zo" "../base/zo-shell_rkt.zo" "../base/zo-string_rkt.zo" "../base/zo-transition_rkt.zo"))

(define (parse-data ps*)
  (map zo-read ps*))

(define (main ps*)
  (define zo* (parse-data ps*))
  (time
    (for ((zo (in-list zo*)))
      (init (vector zo "branch")))))

(main TYPED-DATA)
