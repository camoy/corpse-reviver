#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require scriblib/autobib
         "bib-util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cite definition

(define-cite ~cite citet generate-bibliography
  #:style number-style
  #:cite-author cite-author
  #:cite-year cite-year)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bibliography

(define siek-2006
  (make-bib
   #:title "Gradual Typing for Functional Languages"
   #:author (authors (author-name "Jeremy G." "Siek")
                     "Walid Taha")
   #:location (proceedings-location scheme-workshop #:pages '(81 92))
   #:date 2006))

(define tobin-hochstadt-2006
  (make-bib
   #:title "Interlanguage Migration: From Scripts to Programs"
   #:author (authors "Sam Tobin-Hochstadt"
                     "Matthias Felleisen")
   #:location (proceedings-location oopsla #:pages '(964 974))
   #:date 2006))

(define tobin-hochstadt-2008
  (make-bib
   #:title "The Design and Implementation of Typed Scheme"
   #:author (authors "Sam Tobin-Hochstadt"
                     "Matthias Felleisen")
   #:location (proceedings-location popl #:pages '(395 406))
   #:date 2008))

(define typescript-2012
  (make-bib
   #:title "TypeScript - JavaScript that scales."
   #:author "Microsoft Corporation"
   #:date "2012"
   #:url "https://www.typescriptlang.org/"))

(define sorbet-2019
  (make-bib
   #:title "Sorbet · A static type checker for Ruby"
   #:author "Stripe, Inc."
   #:date "2019"
   #:url "https://sorbet.org/"))

(define chaudhuri-2017
  (make-bib
    #:title "Fast and Precise Type Checking for JavaScript"
    #:author (authors "Avik Chaudhuri"
                      "Panagiotis Vekris"
                      "Sam Goldman"
                      "Marshall Roch"
                      "Gabriel Levi")
    #:location (journal-location pacmpl
                                 #:volume "1"
                                 #:number "OOPSLA"
                                 #:pages '(48:1 48:30))
    #:date 2019))

(define findler-2002
  (make-bib
   #:title "Contracts for Higher-Order Functions"
   #:author (authors (author-name "Robert Bruce" "Findler")
                     "Matthias Felleisen")
   #:location (proceedings-location icfp #:pages '(48 59))
   #:date 2008))

(define takikawa-2016
  (make-bib
   #:title "Is Sound Gradual Typing Dead?"
   #:author (authors "Asumu Takikawa"
                     "Daniel Feltey"
                     "Ben Greenman"
                     (author-name "Max S." "New")
                     "Jan Vitek"
                     "Matthias Felleisen")
   #:location (proceedings-location popl #:pages '(456 468))
   #:date 2016))

(define greenman-2019
  (make-bib
   #:title "How to Evaluate the Performance of Gradual Typing Systems"
   #:author (authors "Ben Greenman"
                     "Asumu Takikawa"
                     (author-name "Max S." "New")
                     "Daniel Feltey"
                     (author-name "Robert Bruce" "Findler")
                     "Jan Vitek"
                     "Matthias Felleisen")
   #:location (journal-location jfp
                                #:volume "29"
                                #:number "e4")
   #:date 2019))

(define kuhlenschmidt-2019
  (make-bib
   #:title "Toward Efficient Gradual Typing for Structural Types via Coercions"
   #:author (authors "Andre Kuhlenschmidt"
                     "Deyaaeldeen Almahallawi"
                     (author-name "Jeremy G." "Siek"))
   #:location (proceedings-location pldi #:pages '(517 532))
   #:date 2019))

(define greenman-2018
  (make-bib
   #:title "A Spectrum of Type Soundness and Performance"
   #:author (authors "Ben Greenman"
                     "Matthias Felleisen")
   #:location (journal-location pacmpl
                                #:volume "2"
                                #:number "ICFP"
                                #:pages '(71:1 71:32))
   #:date 2018))

(define lehtosalo-2017
  (make-bib
   #:title "mypy: Optional Static Typing for Python"
   #:author "Jukka Lehtosalo"
   #:date "2017"
   #:url "http://mypy-lang.org/"))

(define black-2012
  (make-bib
   #:title "Grace: The Absence of (Inessential) Difficulty"
   #:author (authors (author-name "Andrew P." "Black")
                     (author-name "Kim B." "Bruce")
                     "Michael Homer"
                     "James Noble")
   #:location (proceedings-location onward #:pages '(85 98))
   #:date 2012))

(define vitousek-2014
  (make-bib
   #:author (authors (author-name "Michael M." "Vitousek")
                     "Andrew Kent"
                     (author-name "Jeremy G." "Siek")
                     "Jim Baker")
   #:title "Design and Evaluation of Gradual Typing for Python"
   #:location (proceedings-location dls #:pages '(45 56))
   #:date 2014))

(define rastogi-2015
  (make-bib
   #:author (authors "Aseem Rastogi"
                     "Nikhil Swamy"
                     "Cédric Fournet"
                     "Gavin Bierman"
                     "Panagiotis Vekris")
   #:title "Safe & Efficient Gradual Typing for TypeScript"
   #:location (proceedings-location popl #:pages '(167 180))
   #:date 2015))

(define muehlboeck-2017
  (make-bib
    #:title "Sound Gradual Typing is Nominally Alive and Well"
    #:author (authors "Fabian Muehlboeck"
                      "Ross Tate")
    #:location (journal-location pacmpl
                                 #:volume "1"
                                 #:number "OOPSLA"
                                 #:pages '(56:1 56:30))
    #:date 2017))

(define dart-2011
  (make-bib
   #:title "Dart Programming Language"
   #:author "Google LLC"
   #:date "2011"
   #:url "https://dart.dev/"))

(define wrigstad-2010
  (make-bib
   #:title "Integrating Typed and Untyped Code in a Scripting Language"
   #:author (authors "Tobias Wrigstad"
                     (author-name "Francesco Zappa" "Nardelli")
                     "Sylvain Lebresne"
                     "Johan Östlund"
                     "Jan Vitek")
   #:location (proceedings-location popl #:pages '(377 388))
   #:date 2010))

(define nguyen-2018
  (make-bib
   #:title "Soft Contract Verification for Higher-Order Stateful Programs"
   #:author (authors (author-name "Phúc C." "Nguyễn")
                     "Thomas Gilray"
                     "Sam Tobin-Hochstadt"
                     (author-name "David" "Van Horn"))
   #:location (journal-location pacmpl
                                #:volume "2"
                                #:number "POPL"
                                #:pages '(51:1 51:30))
   #:date 2018))

(define darais-2017
  (make-bib
   #:title "Abstracting Definitional Interpreters (Functional Pearl)"
   #:author (authors "David Darais"
                     "Nicholas Labich"
                     (author-name "Phúc C." "Nguyễn")
                     (author-name "David" "Van Horn"))
   #:location (journal-location pacmpl
                                #:volume "1"
                                #:number "ICFP"
                                #:pages '(12:1 12:25))
   #:date 2017))

(define van-horn-2010
  (make-bib
   #:title "Abstracting Abstract Machines"
   #:author (authors (author-name "David" "Van Horn")
                     "Matthew Might")
   #:location (proceedings-location icfp #:pages '(51 62))
   #:date 2010))
