#lang racket/base

(require file/resource
         "../../../cldr/core.rkt"
         "env.rkt"
         "windows-registry.rkt")

(provide detect-tzid/windows)

(define (detect-tzid/windows)
  (or (tzid-from-env)
      (tzid-from-registry/vista)
      (tzid-from-registry/nt)
      (tzid-from-registry/95)))

(define (tzid-from-registry/vista)
  (windows->tzid
   (get-resource KEY (string-append TZINFO-KEY "\\TimeZoneKeyName"))))

(define (tzid-from-registry/nt)
  (windows->tzid
   (tzid-from-registry-list "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Time Zones")))

(define (tzid-from-registry/95)
  (windows->tzid
   (tzid-from-registry-list "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Time Zones")))

(define (windows->tzid tz)
  (and tz
       (for*/first ([mz (in-list (windows-zones))]
                    [win (in-value (cldr-ref mz '(mapZone _other)))]
                    #:when (equal? tz win))
         (cldr-ref mz '(mapZone _type)))))

(define (tzid-from-registry-list prefix)
  (define standard (standard-name))
  (define tzs (subresources KEY prefix))
  
  (for*/first ([tz (in-list tzs)]
               [std (in-value (get-resource KEY (format "~a\\~a\\Std" prefix tz)))]
               #:when (equal? standard std))
    tz))

(define (standard-name)
  (get-resource KEY (string-append TZINFO-KEY "\\StandardName")))
  

(define KEY "HKEY_LOCAL_MACHINE")
(define TZINFO-KEY "SYSTEM\\CurrentControlSet\\Control\\TimeZoneInformation")
