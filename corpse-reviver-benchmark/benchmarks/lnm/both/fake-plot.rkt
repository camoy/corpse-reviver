#lang racket/base

(require pict
         plot/pict
         (only-in plot/utils linear-seq))

(provide
  (struct-out pre-tick)
  (struct-out tick)
  (struct-out ticks)
  (struct-out renderer2d)
  pict?
  line-width
  plot-width
  plot-height
  plot-x-ticks
  plot-y-ticks
  plot-x-far-ticks
  plot-y-far-ticks
  plot-font-face
  plot-font-size
  no-ticks
  linear-seq
  (rename-out [-function function]
              [-plot-pict plot-pict]
              [-lines lines]))

(define (-function f x-min x-max samples color width)
  (function f x-min x-max
            #:samples samples
            #:color color
            #:width width))

(define (-plot-pict renderers x-min x-max y-min y-max x-label y-label width height)
  (plot-pict renderers
             #:x-min x-min
             #:x-max x-max
             #:y-min y-min
             #:y-max y-max
             #:x-label x-label
             #:y-label y-label
             #:width width
             #:height height))

(define (-lines vs c w s)
  (lines vs #:color c #:width w #:style s))
