#lang racket/base

(require racket/contract/base
         "private/generics.rkt"
         "private/structs.rkt")

(provide/contract
 [tzinfo-source?             (-> any/c boolean?)]
 [tzinfo->all-tzids          (-> tzinfo-source? (listof string?))]
 [tzinfo-has-tzid?           (-> tzinfo-source? string? boolean?)]
 [tzinfo-tzid->country-codes (-> tzinfo-source? string? (listof string?))]
 [tzinfo-country-code->tzids (-> tzinfo-source? string? (listof string?))]
 [seconds->tzoffset/utc      (-> tzinfo-source? string? real? tzoffset?)]
 [seconds->tzoffset/local    (-> tzinfo-source? string? real? (or/c tzoffset? tzgap? tzoverlap?))]
 [detect-system-tzid         (-> tzinfo-source? (or/c string? #f))])

(provide gen:tzinfo-source)
