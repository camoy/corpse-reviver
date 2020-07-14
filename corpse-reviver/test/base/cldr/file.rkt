#lang racket/base

(require racket/dict
         racket/match
         racket/port
         racket/runtime-path
         file/unzip
         json)

(provide cldr-ref
         cldr-ref*
         cldr-json)

(define (cldr-ref json key [fail (λ ()
                                   (raise (exn:fail:contract
                                           (format "cldr-ref: no value found for key\n\tkey : ~s" key)
                                           (current-continuation-marks))))])
  (define (return/fail) (if (procedure? fail) (fail) fail))
  
  (cond [(list? key)
         (let loop ([json json] [key key])
           (match key
             [(cons k key)
              (match (dict-ref json (symbolize-key k) #f)
                [#f (return/fail)]
                [j  (loop j key)])]
             [(list) json]))]
        [else
         (dict-ref json (symbolize-key key) fail)]))

(define (cldr-ref* #:fail fail json . keys)
  (or
   (for*/first ([key (in-list keys)]
                [res (in-value (cldr-ref json key #f))]
                #:when res)
     res)
   (if (procedure? fail)
       (fail)
       fail)))

(define (cldr-json zip-path pkg path prefix #:cache? [cache? #t])
  (define ref (if cache? hash-ref! hash-ref))

  (ref JSON-CACHE
       path
       (λ ()
         (cldr-ref
          (load-json zip-path (build-path pkg path))
          prefix))))

(define (symbolize-key k)
  (cond [(symbol? k) k]
        [(string? k) (string->symbol k)]
        [(integer? k) (string->symbol (number->string k))]))

(define (load-json zip-path data-path)
  (call-with-input-file* zip-path
    (λ (in)
      (define zip-path (path->zip-path data-path))
      (define dir (read-zip-directory in))
      
      (unless (zip-directory-contains? dir zip-path)
        (error
         (format "CLDR file not found: ~a" zip-path)))
          
      (define-values (pipe-in pipe-out) (make-pipe))
      (define reader (make-extracting-entry-reader pipe-out zip-path))
      (unzip-entry in dir zip-path reader)
      (read-json pipe-in))))

(define (make-extracting-entry-reader out zip-path)
  (λ (name dir? in)
    (when dir?
      (error
       (format "CLDR path names a directory, not a file: ~a" zip-path)))
    
    (copy-port in out)))


(define JSON-CACHE (make-hash))