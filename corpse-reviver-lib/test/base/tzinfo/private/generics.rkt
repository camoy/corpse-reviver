#lang racket/base

(require racket/generic)

(provide (all-defined-out))

(define-generics tzinfo-source
  (tzinfo->all-tzids          tzinfo-source)
  (tzinfo-has-tzid?           tzinfo-source tzid)
  (tzinfo-tzid->country-codes tzinfo-source tzid)
  (tzinfo-country-code->tzids tzinfo-source cc)
  (seconds->tzoffset/utc      tzinfo-source tzid seconds)
  (seconds->tzoffset/local    tzinfo-source tzid seconds)
  (detect-system-tzid         tzinfo-source))

