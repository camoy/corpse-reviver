#lang racket/base

(require racket/string)
(provide str)
(define str (string-join '("hello" " " "world")))
