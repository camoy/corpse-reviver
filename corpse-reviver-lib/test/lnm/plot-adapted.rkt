#lang typed/racket/base

(require corpse-reviver/opaque)

(define-type Plot-Pen-Style-Sym
  (U 'transparent 'solid    'dot 'long-dash
     'short-dash  'dot-dash))

(define-type Plot-Pen-Style
  (U Integer Plot-Pen-Style-Sym))

(provide Plot-Pen-Style)

(require/typed/provide/opaque "_plot.rkt"
  [#:struct pre-tick ([value : Real] [major? : Boolean])]
  [#:struct (tick pre-tick) ([label : String])]
  [#:struct ticks ([layout : (-> Real Real (Listof pre-tick))]
                   [format : (-> Real Real (Listof pre-tick) (Listof String))])]
  [#:opaque renderer2d renderer2d?]
  [#:opaque Pict pict?]
  [line-width (-> Nonnegative-Real)]
  [plot-width (-> Positive-Integer)]
  [plot-height (-> Positive-Integer)]
  [plot-x-ticks (-> ticks Void)]
  [plot-y-ticks (-> ticks Void)]
  [plot-x-far-ticks (-> ticks Void)]
  [plot-y-far-ticks (-> ticks Void)]
  [plot-font-face   (-> String Void)]
  [plot-font-size   (-> Nonnegative-Real Void)]
  [no-ticks ticks]
  [linear-seq (-> Real Real Nonnegative-Integer (Listof Real))] 
  [function
   (-> (-> Real Real)
       (U Real #f)
       (U Real #f)
       Positive-Integer
       Symbol
       Nonnegative-Real
       renderer2d)]
  [plot-pict
   (-> (Listof renderer2d)
       (U Real #f)
       (U Real #f)
       (U Real #f)
       (U Real #f)
       (U String #f)
       (U String #f)
       Positive-Integer
       Positive-Integer
       Pict)]
  [lines
   (-> (Listof (Listof Real))
       Symbol
       Nonnegative-Real
       Plot-Pen-Style
       renderer2d)])
