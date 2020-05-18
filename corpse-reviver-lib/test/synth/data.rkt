#lang typed/racket/base

(provide
  (struct-out Array)
  (struct-out Settable-Array)
  (struct-out Mutable-Array))

  (struct Array ([shape : (Vectorof Integer)]
                   [size : Integer]
                   [strict? : (Boxof Boolean)]
                   [strict! : (-> Void)]
                   [unsafe-proc : (-> (Vectorof Integer) Float)]))
  (struct Settable-Array Array ([set-proc : ((Vectorof Integer) Float -> Void)]))
  (struct Mutable-Array Settable-Array ([data : (Vectorof Float)]))

