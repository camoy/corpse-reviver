#lang racket/base

(require racket/file
         racket/match
         racket/path
         racket/string
         "env.rkt")

(provide detect-tzid/unix)

(define (detect-tzid/unix zoneinfo-dir default-zoneinfo-dir all-tzids)
  (or (tzid-from-env)
      (and zoneinfo-dir
           (tzid-from-/etc/localtime zoneinfo-dir default-zoneinfo-dir all-tzids))
      (tzid-from-/etc/timezone)
      (tzid-from-/etc/TIMEZONE)
      (tzid-from-/etc/sysconfig/clock)
      (tzid-from-/etc/default/init)))


(define (tzid-from-/etc/localtime zoneinfo-dir default-zoneinfo-dir all-tzids)
  (define /etc/localtime "/etc/localtime")
  (define base-path (resolve-path zoneinfo-dir))

  (define (find-matching-zone)
    (define size (file-size /etc/localtime))
    (define content (file->bytes /etc/localtime))

    (for*/first ([tzid (in-list all-tzids)]
                 [f (in-value (build-path base-path tzid))]
                 #:when (and (= (file-size f) size)
                             (equal? (file->bytes f) content)))
      tzid))

  (and (file-exists? /etc/localtime)
       default-zoneinfo-dir
       (let ([rel (find-relative-path default-zoneinfo-dir (normalize-path /etc/localtime "/etc"))])
         (match (explode-path rel)
           [(cons 'up _) (find-matching-zone)]
           [_ (path->string rel)]))))

(define (tzid-from-/etc/timezone)
  (define /etc/timezone "/etc/timezone")

  (and (file-exists? /etc/timezone)
       (string-trim (file->string /etc/timezone))))

(define (tzid-from-/etc/TIMEZONE)
  (define /etc/TIMEZONE "/etc/TIMEZONE")

  (tzid-from-var /etc/TIMEZONE "TZ"))

(define (tzid-from-/etc/sysconfig/clock)
  (define /etc/sysconfig/clock "/etc/sysconfig/clock")

  (tzid-from-var /etc/sysconfig/clock "(?:TIMEZONE|ZONE)"))

(define (tzid-from-/etc/default/init)
  (define /etc/default/init "/etc/default/init")

  (tzid-from-var /etc/default/init "TZ"))

(define (tzid-from-var file var)
  (define re (pregexp (string-append "^\\s*" var "\\s*=\\s*(\\S+)")))

  (and (file-exists? file)
       (for*/last ([s (in-list (file->lines file))]
                   [m (in-value (regexp-match re s))]
                   #:when m)
         (cadr m))))
