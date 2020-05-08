#lang racket/base

(require racket/contract/base
         racket/match
         racket/string
         racket/set
         memoize)

(require "core.rkt")

(struct cldr-locale
  (lang script region)
  #:transparent)

(provide (struct-out cldr-locale))

(provide/contract
 [locale->available-cldr-locale (-> string? (-> string? boolean?) string?)] 
 
 [parse-locale             (-> string? cldr-locale?)]
 [locale->cldr-locale      (-> string? cldr-locale?)]
 [locale->cldr-language    (-> string? string?)]
 [locale->cldr-region      (-> string? string?)]
 [locale->cldr-script      (-> string? string?)])


(define/memo* (locale->available-cldr-locale locale available?)
  (define full (locale->cldr-locale locale))
  
  (for/first ([str (in-list (locale->possible-strings full))]
              #:when (available? str))
    str))

(define (locale->cldr-language str) (cldr-locale-lang (locale->cldr-locale str)))
(define (locale->cldr-region str)   (cldr-locale-region (locale->cldr-locale str)))
(define (locale->cldr-script str)   (cldr-locale-script (locale->cldr-locale str)))


(define (locale->cldr-locale str)
  (hash-ref cldr-locale-cache
            str
            (λ ()
              (define full (resolve-locale (parse-locale str)))
              (hash-set! cldr-locale-cache str full)
              full)))

(define locale-path-cache (make-hash))
(define cldr-locale-cache (make-hash))

(define (locale->possible-strings l)
  (match l
    [(cldr-locale lang script region)
     (list (format "~a-~a-~a" lang script region)
           (format "~a-~a" lang region)
           lang
           "root")]))

(define (cldr-database-locale src-locale)
  (define full (locale->cldr-locale src-locale))
  (for/first ([str (in-list (locale->possible-strings full))]
              #:when (set-member? (modern-locales) str))
    str))

(define (parse-locale str)
  (match (string-trim str)
    [(regexp #px"^([^-_.]+)(?:[-_])([^-_.]+)(?:[-_])([^-_.]+)" (list _ lang script region))
     (build-locale lang script region)]
    [(regexp #px"^([^-_.]+)(?:[-_])([^-_.]+)" (list _ lang region))
     (build-locale lang #f region)]
    [(regexp #px"^[a-zA-Z]+" (list lang))
     (build-locale lang #f #f)]
    [_
     (build-locale "und" #f #f)]))

(define (build-locale lang script region)
  (cldr-locale (and lang   (string-downcase lang))
               (and script (string-titlecase script))
               (and region (string-upcase region))))

(define (resolve-locale l)
  (define (lookup src) (cldr-ref (likely-subtags) src #f))
  
  (match l
    [(cldr-locale lang script region)
     (define likely
       (parse-locale
        (or (and lang script region
                 (format "~a-~a-~a" lang script region))
            (and lang region
                 (lookup (format "~a-~a" lang region)))
            (and lang script
                 (lookup (format "~a-~a" lang script)))
            (and lang
                 (lookup (format "~a" lang)))
            (and script
                 (lookup (format "und-~a" script)))
            (lookup "und"))))
     (match likely
       [(cldr-locale def-lang def-script def-region)
        (cldr-locale (or (and (not (equal? lang "und")) lang) def-lang)
                     (or script def-script)
                     (or region def-region))])]))
