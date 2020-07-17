#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide ut-require-ty
         ty-require-ut)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (for-syntax racket/base)
         ppict/pict
         ppict/tag
         racket/math
         pict
         pict-abbrevs
         racket/runtime-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define STAGE-WIDTH 800)
(define STAGE-HEIGHT 825)
(define TYPED-COLOR "bisque")
(define UNTYPED-COLOR "lavender")
(define MODULE-WIDTH 120)
(define MODULE-HEIGHT 130)
(define CONTRACT-WIDTH (/ MODULE-WIDTH 8))
(define CONTRACT-COLOR "blue")
(define ARROW-SIZE 10)
(define LABEL-FONT "Fira Code")
(define LABEL-SIZE 18)
(define LABEL-BACKGROUND-COLOR "white")
(define LINE-WIDTH-CONVERT 5)
(define LABEL-FUDGE -55)
(define HOLE-LOWER 8)
(define HOLE-UPPER 10)
(define HOLE-NUMBER-LOWER 8)
(define HOLE-NUMBER-UPPER 12)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (annotate txt)
  (colorize
   (text txt
         (cons 'italic 'roman)
         (ceiling (* 9/10 LABEL-SIZE)))
   "dimgray"))

(define (labeled-module width
                        height
                        kind
                        name)
  (define color (if (eq? kind 'typed) TYPED-COLOR UNTYPED-COLOR))
  (ppict-do
   (file-icon width height color #t)
   #:go (coord 1/2 1/2 'cc)
   (text name LABEL-FONT (ceiling (* width 17/100)))))

(define (poke-holes base n color)
  (clip
   (for/fold ([acc base])
             ([_ (in-range n)])
     (ppict-do
      acc
      #:go (coord 1/2 (random) 'cc)
      (filled-rectangle
       CONTRACT-WIDTH
       (random HOLE-LOWER HOLE-UPPER)
       #:draw-border? #f
       #:color color)))))

(define ((filled-rectangle-holes hole-color) w h #:color color #:draw-border? _)
  (poke-holes (filled-rectangle w h #:color hole-color #:draw-border? #f)
              (random HOLE-NUMBER-LOWER HOLE-NUMBER-UPPER)
              color))

(define (contracted p #:hole-color [hole-color #f])
  (define contracts
    (filled-rectangle CONTRACT-WIDTH (pict-height p) #:color CONTRACT-COLOR #:draw-border? #f))
  (hc-append contracts p))

(define (label txt orient [color #f])
  (define f (if color (λ (p) (colorize p color)) values))
  (f (text txt LABEL-FONT LABEL-SIZE (if (eq? orient 'lr) 0 (/ pi 2)))))

(define ae
  (hc-append
   (text "analyze" LABEL-FONT LABEL-SIZE (/ pi 2))
   (text "erase" LABEL-FONT LABEL-SIZE (/ pi 2))))

(define (arrow p first-tag second-tag text
               #:adjust [adjust LABEL-FUDGE]
               #:style [style #f]
               #:orient [orient 'lr]
               #:different-label [dl #f]
               #:unsafe [unsafe #f])
  (pin-arrow-line
   ARROW-SIZE
   p
   (find-tag p first-tag)
   (λ z
     (define f (if (eq? orient 'lr) rc-find cb-find))
     (define-values (x y) (apply f z))
     (values x (if unsafe (+ 30 y) y)))
   (find-tag p second-tag)
   (λ z
     (define f
       (cond
         [unsafe cc-find]
         [(eq? orient 'lr) lc-find]
         [else ct-find]))
     (define-values (x y) (apply f z))
     (values x (if unsafe (+ 30 y) y)))
   #:color (if unsafe "red" #f)
   #:under? #f
   #:style style
   #:line-width
   (if (eq? orient 'lr) 1 LINE-WIDTH-CONVERT)
   #:label (or dl (label text orient (and unsafe "red")))
   #:x-adjust-label (if (eq? orient 'lr) 0 adjust)))

(define base
  (blank STAGE-WIDTH STAGE-HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ut-require-ty
  (let ()
    (define main
      (labeled-module MODULE-WIDTH MODULE-HEIGHT 'untyped "main"))
    (define streams
      (labeled-module MODULE-WIDTH MODULE-HEIGHT 'typed "streams"))
    (define pre-main
      (labeled-module MODULE-WIDTH MODULE-HEIGHT 'untyped "main"))
    (define pre-streams
      (contracted
       (labeled-module MODULE-WIDTH MODULE-HEIGHT 'untyped "streams")))
    (define final-streams
      (labeled-module MODULE-WIDTH MODULE-HEIGHT 'typed "streams"))

    (ppict-do
     base
     #:go (coord 1/20 1/10 'lt) (annotate "source")
     #:go (coord 1/4 1/5 'cc) (tag-pict main 'src-main)
     #:go (coord 3/4 1/5 'cc) (tag-pict streams 'src-streams)

     #:go (coord 1/4 1/2 'cc) (tag-pict pre-main 'pre-main)
     #:go (coord 3/4 1/2 'cc) (tag-pict pre-streams 'pre-streams)

     #:go (coord 1/20 0.7 'lt) (annotate "output")
     #:go (coord 1/4 4/5 'cc) (tag-pict pre-main 'final-main)
     #:go (coord 3/4 4/5 'cc) (tag-pict final-streams 'final-streams)

     #:set (arrow ppict-do-state 'src-main 'src-streams "require")
     #:set (arrow ppict-do-state 'pre-main 'pre-streams "require")
     #:set (arrow ppict-do-state 'final-main 'final-streams "require")
     #:set (arrow ppict-do-state 'final-main 'final-streams "require" #:unsafe #t)

     #:set (arrow ppict-do-state 'src-main 'pre-main "unchanged" #:style 'long-dash #:orient 'ud #:adjust -65)
     #:set (arrow ppict-do-state 'pre-main 'final-main "analyze" #:orient 'ud #:adjust -73)
     #:set (arrow ppict-do-state 'pre-main 'final-main "bypass" #:orient 'ud #:adjust -50)

     #:set (arrow ppict-do-state 'src-streams 'pre-streams "explicate" #:orient 'ud #:adjust -65)
     #:set (arrow ppict-do-state 'pre-streams 'final-streams "analyze" #:orient 'ud #:adjust -52)
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ty-require-ut
  (let ()
    (define main
      (labeled-module MODULE-WIDTH MODULE-HEIGHT 'typed "main"))
    (define streams
      (labeled-module MODULE-WIDTH MODULE-HEIGHT 'untyped "streams"))
    (define after-main
      (labeled-module MODULE-WIDTH MODULE-HEIGHT 'untyped "main"))
    (define after-streams
      (labeled-module MODULE-WIDTH MODULE-HEIGHT 'untyped "streams"))
    (define proxy
      (contracted (labeled-module (* CONTRACT-WIDTH 3) MODULE-HEIGHT 'untyped "")))
    (define final-proxy
      (contracted (labeled-module (* CONTRACT-WIDTH 3) MODULE-HEIGHT 'untyped "")
                  #:hole-color UNTYPED-COLOR))

    (ppict-do
     base
     #:go (coord 1/20 1/10 'lt) (annotate "source")
     #:go (coord 1/4 1/5 'cc) (tag-pict main 'src-main)
     #:go (coord 3/4 1/5 'cc) (tag-pict streams 'src-streams)

     #:go (coord 1/4 1/2 'cc) (tag-pict after-main 'pre-main)
     #:go (coord 3/4 1/2 'cc) (tag-pict after-streams 'pre-streams)
     #:go (coord 1/2 1/2 'cc) (tag-pict proxy 'pre-proxy)

     #:go (coord 1/20 0.7 'lt) (annotate "output")
     #:go (coord 1/4 4/5 'cc) (tag-pict main 'final-main)
     #:go (coord 3/4 4/5 'cc) (tag-pict after-streams 'final-streams)
     #:go (coord 1/2 4/5 'cc) (tag-pict final-proxy 'final-proxy)

     #:set (arrow ppict-do-state 'src-main 'src-streams "require/typed")
     #:set (arrow ppict-do-state 'pre-main 'pre-proxy "require")
     #:set (arrow ppict-do-state 'pre-proxy 'pre-streams "require")
     #:set (arrow ppict-do-state 'final-main 'final-proxy "require")
     #:set (arrow ppict-do-state 'final-main 'final-proxy "require" #:unsafe #t)
     #:set (arrow ppict-do-state 'final-proxy 'final-streams "require")

     #:set (arrow ppict-do-state 'src-main 'pre-main "explicate" #:orient 'ud #:adjust -65)
     #:set (arrow ppict-do-state 'src-streams 'pre-streams "unchanged" #:style 'long-dash #:orient 'ud #:adjust -65)
     #:set (arrow ppict-do-state 'pre-main 'final-main "analyze" #:orient 'ud #:adjust -75)
     #:set (arrow ppict-do-state 'pre-main 'final-main "bypass" #:orient 'ud #:adjust -48)
     #:set (arrow ppict-do-state 'pre-proxy 'final-proxy "unchanged" #:style 'long-dash #:orient 'ud #:adjust -65)
     #:set (arrow ppict-do-state 'pre-streams 'final-streams "analyze" #:orient 'ud #:adjust -53)
     )))
