#lang racket/base

(provide ut-require-ty
         ty-require-ut)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (only-in metapict save-pict)
         racket/draw
         racket/class
         racket/math
         ppict/2
         pict
         (except-in pict-abbrevs save-pict))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define WIDTH 700)
(define HEIGHT 925)

(define MONO "Fira Code")
(define SERIF "Linux Libertine")
(define SIZE 28)

(define ARROW-COLOR "lightgray")
(define ARROW-TEXT-COLOR "dimgray")

(define MOD-WIDTH 135)
(define MOD-RATIO 4/3)
(define PROXY-WIDTH 40)
(define PROXY-HEIGHT (* MOD-WIDTH MOD-RATIO))
(define MOD-BORDER-WIDTH 2)
(define MOD-LINES 10)
(define CONTRACT-WIDTH 15)

(define MOD-ARROW-FONT-SIZE 24)
(define MOD-ARROW-SIZE 25)
(define MOD-ARROW-WIDTH 8)
(define MOD-ARROW-LABEL-Y-ADJUST -10)

(define MOD-US-COLOR (hex-triplet->color% #xff4d6a))
(define MOD-US-TEXT (hex-triplet->color% #xcc2844))

(define MOD-UT-COLOR (hex-triplet->color% #xf7fbfc))
(define MOD-UT-BORDER (hex-triplet->color% #xd6f4fc #;#xb9d7ea))
(define MOD-UT-TEXT (hex-triplet->color% #x769fcd))
(define MOD-UT-LINE (hex-triplet->color% #xd6f4fc))

(define MOD-TY-COLOR (hex-triplet->color% #xffd6b2))
(define MOD-TY-BORDER (hex-triplet->color% #xffc18c #;#xff884c))
(define MOD-TY-TEXT (hex-triplet->color% #xff884c))
(define MOD-TY-LINE (hex-triplet->color% #xffc18c))

;; ff4d4d

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (text* str
               #:size [size SIZE]
               #:family [family MONO]
               #:color [color #f])
  (define t0
    (text str family size))
  (define t1
    (if color (colorize t0 color) t0))
  t1)

(define (mod-lines #:width [w MOD-WIDTH]
                   #:height [h (* w MOD-RATIO)]
                   #:lines [lines MOD-LINES]
                   #:color [color #f])
  (define lines-pict
    (for/fold ([p (blank w h)])
              ([k (in-range 1 lines)])
      (ppict-do p
                #:go (coord 1/2 (/ k lines) 'cc)
                (rule w
                      MOD-BORDER-WIDTH
                      #:color color))))
  (scale lines-pict 4/5 1))

(define (mod str
             #:width [w MOD-WIDTH]
             #:height [h (* w MOD-RATIO)]
             #:color [color #f]
             #:border [border-color #f]
             #:text [text-color #f]
             #:line [line-color #f])
  (ppict-do (filled-rectangle w
                              h
                              #:color color
                              #:draw-border? border-color
                              #:border-color border-color
                              #:border-width MOD-BORDER-WIDTH)
            #:go (coord 1/2 1/2 'cc)
            (mod-lines #:color line-color #:width w #:height h)
            #:go (coord 1/2 1/2 'cc)
            (if str
                (add-rectangle-background
                 (text* str #:color text-color)
                 #:radius 0
                 #:color color
                 #:x-margin 10)
                (blank))))

(define (mod-ut str tag
                #:width [w MOD-WIDTH]
                #:height [h (* w MOD-RATIO)]
                #:contract? [contract? #f])
  (define contract-pict
    (filled-rectangle CONTRACT-WIDTH
                      h
                      #:color MOD-UT-TEXT
                      #:draw-border? #f))
  (define base
    (mod str
         #:width w
         #:height h
         #:color MOD-UT-COLOR
         #:border MOD-UT-BORDER
         #:text MOD-UT-TEXT
         #:line MOD-UT-LINE))
  (tag-pict
   (if contract?
       (hc-append contract-pict base)
       base)
   tag))

(define (mod-ty str tag)
  (tag-pict
   (mod str
        #:color MOD-TY-COLOR
        #:border MOD-TY-BORDER
        #:text MOD-TY-TEXT
        #:line MOD-TY-LINE)
   tag))

(define (pict-material p s c)
  (dc (λ (dc dx dy)
        (define old-brush (send dc get-brush))
        (send dc set-brush (new brush% [style s] [color c]))
        (draw-pict p dc 0 0)
        (send dc set-brush old-brush))
      (pict-width p)
      (pict-height p)))

(define (mod-arrow p str from to
                   #:text text-color
                   #:color color)
  (pin-arrow-line
   MOD-ARROW-SIZE p
   #:label (text* str
                  #:size MOD-ARROW-FONT-SIZE
                  #:color text-color)
   #:y-adjust-label MOD-ARROW-LABEL-Y-ADJUST
   #:line-width MOD-ARROW-WIDTH
   #:under? #t
   #:color color
   (find-tag p from) rc-find
   (find-tag p to) lc-find))

(define-syntax-rule (mod-ty-arrow str from to)
  (mod-arrow ppict-do-state str from to
             #:text ARROW-TEXT-COLOR
             #:color ARROW-COLOR))

(define-syntax-rule (mod-ut-arrow str from to)
  (mod-arrow ppict-do-state str from to
             #:text ARROW-TEXT-COLOR
             #:color ARROW-COLOR))

(define-syntax-rule (mod-us-arrow str from to)
  (mod-arrow ppict-do-state str from to
             #:text MOD-US-TEXT
             #:color MOD-US-COLOR))

(define-syntax-rule (wavy-arrow str from to x-adjust?)
  (let ([strs (if (list? str) str (list str))]
        [p ppict-do-state])
    (pin-arrow-line
     MOD-ARROW-SIZE p
     #:label
     (apply
      vc-append
      (for/list ([s (in-list strs)])
        (text* s
               #:size MOD-ARROW-FONT-SIZE
               #:color ARROW-TEXT-COLOR)))
     #:under? #t
     #:line-width MOD-ARROW-WIDTH
     #:x-adjust-label
     (if x-adjust? (- -3 (/ MOD-ARROW-FONT-SIZE 2)) 0)
     #:y-adjust-label MOD-ARROW-LABEL-Y-ADJUST
     #:start-angle -2.1
     #:end-angle -2
     #:color ARROW-COLOR
     (find-tag p from) cb-find
     (find-tag p to) ct-find)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (background c r str)
  (ppict-do (blank (/ WIDTH c) (/ HEIGHT r))
            #:go (coord 0 0 'lt)
            (text* str #:family SERIF)))

(define (mod-tile content c r [name ""])
  (ppict-do (background c r name)
            #:go (coord 1/2 1/2 'cc)
            content))

(define ut-require-ty
  (ppict-do (blank WIDTH HEIGHT)
            #:go (tile 2 3)

            (mod-tile (mod-ut "main" 'c00) 2 3 "source")
            (mod-tile (mod-ty "streams" 'c10) 2 3)

            (mod-tile (mod-ut "main" 'c01) 2 3)
            (mod-tile (mod-ut "streams" 'c11 #:contract? #t) 2 3)

            (mod-tile (mod-ut "main" 'c02) 2 3 "target")
            (mod-tile (mod-ty "streams" 'c12) 2 3)

            #:set (mod-ut-arrow "require" 'c00 'c10)
            #:set (mod-ut-arrow "require" 'c01 'c11)
            #:set (mod-us-arrow "require" 'c02 'c12)

            #:set (wavy-arrow "same" 'c00 'c01 #f)
            #:set (wavy-arrow '("analyze" "bypass") 'c01 'c02 #t)
            #:set (wavy-arrow "explicate" 'c10 'c11 #f)
            #:set (wavy-arrow "analyze" 'c11 'c12 #f)
            ))

(define ty-require-ut
  (ppict-do (blank WIDTH HEIGHT)
            #:go (tile 3 3)

            (mod-tile (mod-ty "main" 'c00) 3 3 "source")
            (blank)
            (mod-tile (mod-ut "streams" 'c20) 3 3)

            (mod-tile (mod-ut "main" 'c01) 3 3)
            (mod-tile (mod-ut #f 'c11
                              #:contract? #t
                              #:width PROXY-WIDTH
                              #:height PROXY-HEIGHT) 3 3)
            (mod-tile (mod-ut "streams" 'c21) 3 3)

            (mod-tile (mod-ty "main" 'c02) 3 3 "target")
            (mod-tile (mod-ut #f 'c12
                              #:contract? #t
                              #:width PROXY-WIDTH
                              #:height PROXY-HEIGHT) 3 3)
            (mod-tile (mod-ut "streams" 'c22) 3 3)

            #:set (mod-ty-arrow "require/typed" 'c00 'c20)
            #:set (mod-ut-arrow "require" 'c01 'c11)
            #:set (mod-ut-arrow "require" 'c11 'c21)
            #:set (mod-us-arrow "require" 'c02 'c12)
            #:set (mod-ut-arrow "require" 'c12 'c22)

            #:set (wavy-arrow "explicate" 'c00 'c01 #f)
            #:set (wavy-arrow '("analyze" "bypass") 'c01 'c02 #t)
            #:set (wavy-arrow "same" 'c20 'c21 #f)
            #:set (wavy-arrow "same" 'c11 'c12 #f)
            #:set (wavy-arrow "analyze" 'c21 'c22 #f)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(save-pict "ut-require-ty.pdf"
           ut-require-ty
           'pdf)

#;(save-pict "ty-require-ut.pdf"
           ty-require-ut
           'pdf)
