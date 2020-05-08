#lang racket/base

;; Modified version of `racket/logging` that fixes #:logger.

(require racket/contract/base)

(provide log-level/c)

(define log-level/c (or/c 'none 'fatal 'error 'warning 'info 'debug))
(define log-spec? (listof (or/c symbol? #f)))
(define log-event? (vector-immutable/c log-level/c string? any/c (or/c symbol? #f)))

(provide/contract [with-intercepted-logging
                   (->* ((-> log-event? any)
                         (-> any)
                         log-level/c)
                        (#:logger logger?)
                        #:rest log-spec?
                        any)]
                  [with-logging-to-port
                   (->* (output-port? (-> any) log-level/c)
                        (#:logger logger?)
                        #:rest log-spec?
                        any)])

(define (receiver-thread receiver stop-chan intercept)
  (thread
   (lambda ()
     (define (clear-events)
       (let ([l (sync/timeout 0 receiver)])
         (when l ; still something to read
           (intercept l) ; interceptor gets the whole vector
           (clear-events))))
     (let loop ()
       (let ([l (sync receiver stop-chan)])
         (cond [(eq? l 'stop)
                ;; we received all the events we were supposed
                ;; to get, read them all (w/o waiting), then
                ;; stop
                (clear-events)]
               [else ; keep going
                (intercept l)
                (loop)]))))))

(define (with-intercepted-logging interceptor proc #:logger [logger #f]
                                  . log-spec)
  (define interceptor-key (make-continuation-mark-key))
  (define (filtered-interceptor evt)
    (define cm (vector-ref evt 2))
    (when (continuation-mark-set-first cm interceptor-key)
      (interceptor evt)))
  (let* ([logger    (or logger (current-logger))]
         [receiver  (apply make-log-receiver logger log-spec)]
         [stop-chan (make-channel)]
         [t         (receiver-thread receiver stop-chan filtered-interceptor)])
    (begin0
      (with-continuation-mark interceptor-key #t
        (proc))
      (channel-put stop-chan 'stop) ; stop the receiver thread
      (thread-wait t))))

(define (with-logging-to-port port proc #:logger [logger #f] . log-spec)
  (apply with-intercepted-logging
         #:logger logger
         (lambda (l) (displayln (vector-ref l 1) ; actual message
                                port))
         proc
         log-spec))
