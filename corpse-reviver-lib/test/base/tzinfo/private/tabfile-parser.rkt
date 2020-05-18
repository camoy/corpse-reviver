#lang racket/base

(require racket/contract/base
         racket/match
         racket/string)
(require "structs.rkt")

(provide/contract
 [parse-tabfile (-> path-string? (hash/c string? tabzone?))])

(define (parse-tabfile dir)
  (define path (tabfile-path dir))
  (call-with-input-file* path read-tabfile))

(define (read-tabfile in)
  (define re #px"^([^#\t]+)[\t]([^\t]+)[\t]([^\t]+)(?:[\t](.*))?")
  (for*/hash ([line (in-lines in)]
              [parts (in-value (regexp-match re line))]
              #:when parts)
    (match parts
      [(list _ codes coords tzid comments)
       (values tzid
               (tabzone (string->immutable-string tzid)
                        (map string->immutable-string
                             (string-split codes ","))
                        (parse-coordinates coords)
                        (and comments
                             (string->immutable-string comments))))])))

(define (parse-coordinates str)
  (match str
    [(regexp #px"([-+])(.{2,3})(.{2})(.{2})?([-+])(.{2,3})(.{2})(.{2})?"
             (list _
                   lat-sgn lat-deg lat-min lat-sec
                   lon-sgn lon-deg lon-min lon-sec))
     (coordinates
      (->degrees lat-sgn lat-deg lat-min lat-sec)
      (->degrees lon-sgn lon-deg lon-min lon-sec))]))

(define (->degrees sgn d m s)
  (define absolute
    (+ (string->number d)
       (/ (string->number m) 60)
       (/ (or (and s (string->number s)) 0) 3600)))
  
  (case sgn
    [("+") absolute]
    [else  (- absolute)]))

(define (tabfile-path dir)
  (define paths
    '(("zone1970.tab")
      ("zone.tab")
      ("tab" "zone_sun.tab")))
  
  (for*/first ([suffix (in-list paths)]
               [p (in-value (apply build-path dir suffix))]
               #:when (file-exists? p))
    p))
