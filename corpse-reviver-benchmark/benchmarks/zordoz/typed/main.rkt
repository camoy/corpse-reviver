#lang typed/racket/base

(require corpse-reviver/require-typed-check
         "typed-zo-structs.rkt")

(require/typed/check "zo-shell.rkt"
  [zo-read (-> Path-String zo)]
  [init (-> (Immutable-Vector zo String) Void)])

(define TYPED-DATA
  '("../base/typed-zo-find_rkt.zo"))

(define UNTYPED-DATA
  '("../base/main_rkt.zo" "../base/zo-shell_rkt.zo" "../base/zo-string_rkt.zo" "../base/zo-transition_rkt.zo"))

(: parse-data (-> (Listof Path-String) (Listof zo)))
(define (parse-data ps*)
  (map zo-read ps*))

(: main (-> (Listof Path-String) Void))
(define (main ps*)
  (define zo* (parse-data ps*))
  (time
    (for ((zo (in-list zo*)))
      (init (vector-immutable zo "branch")))))

(main TYPED-DATA)
