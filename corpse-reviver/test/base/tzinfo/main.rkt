#lang racket/base

(require racket/contract/base
         syntax/modresolve)

(require "private/generics.rkt"
         "private/structs.rkt"
         "zoneinfo.rkt")

;; Load the zoneinfo-data package, if it's installed
;; (as it should be on Windows, for example).
(define ZONEINFO-DATA #f)
  ;;(and (file-exists? (resolve-module-path 'tzinfo/zoneinfo-data #f))
  ;;     (dynamic-require 'tzinfo/zoneinfo-data 'ZONEINFO-DATA)))

(provide (struct-out tzoffset)
         (struct-out tzgap)
         (struct-out tzoverlap)
         (struct-out exn:fail:tzinfo)
         (struct-out exn:fail:tzinfo:zone-not-found))

(define istring/c (and/c string? immutable?))

(provide/contract
 [current-tzinfo-source                  (parameter/c (or/c tzinfo-source? false/c))]
 [set-default-tzinfo-source-constructor! (-> (-> tzinfo-source?) void?)]
 [utc-seconds->tzoffset                  (-> string? real? tzoffset?)]
 [local-seconds->tzoffset                (-> string? real? (or/c tzoffset? tzgap? tzoverlap?))]
 [all-tzids                              (-> (listof istring/c))]
 [tzid-exists?                           (-> string? boolean?)]
 [tzid->country-codes                    (-> string? (listof istring/c))]
 [country-code->tzids                    (-> string? (listof istring/c))]
 [system-tzid                            (-> (or/c istring/c false/c))])

(define current-tzinfo-source
  (make-parameter #f))

(define (utc-seconds->tzoffset tzid seconds)
  (seconds->tzoffset/utc (ensure-current-tzinfo-source) tzid seconds))

(define (local-seconds->tzoffset tzid seconds)
  (seconds->tzoffset/local (ensure-current-tzinfo-source) tzid seconds))

(define (all-tzids)
  (tzinfo->all-tzids (ensure-current-tzinfo-source)))

(define (tzid-exists? tzid)
  (tzinfo-has-tzid? (ensure-current-tzinfo-source) tzid))

(define (tzid->country-codes tzid)
  (tzinfo-tzid->country-codes (ensure-current-tzinfo-source) tzid))

(define (country-code->tzids cc)
  (tzinfo-country-code->tzids (ensure-current-tzinfo-source) cc))

(define (ensure-current-tzinfo-source)
  (or (current-tzinfo-source)
      (let ([src (make-default-tzinfo-source)])
        (current-tzinfo-source src)
        src)))

(define (make-default-tzinfo-source)
  (default-tzinfo-source-constructor))

(define (set-default-tzinfo-source-constructor! fn)
  (set! default-tzinfo-source-constructor fn))

(define default-tzinfo-source-constructor (λ () (make-zoneinfo-source)))

(define (system-tzid)
  (unless memoized-system-tzid
    (set! memoized-system-tzid
          (or (detect-system-tzid (ensure-current-tzinfo-source))
              default-system-tzid)))
  
  memoized-system-tzid)

(define memoized-system-tzid #f)
(define default-system-tzid "Etc/UTC")
