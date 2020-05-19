#lang scribble/acmart @acmsmall @review @anonymous

@(require scribble/core
          scribble/html-properties)

@(define style
   (make-style "style" (list (make-css-addition "style.css"))))

@title[#:style style]{
  Corpse Reviver: Sound and Efficient
  Gradual Typing via Contract Verification
}

@author{Cameron Moy}
@author{Phúc C. Nguyễn}
@author{Sam Tobin-Hochstadt}
@author{David Van Horn}

@include-abstract{abstract.scrbl}
@include-section{introduction.scrbl}
@include-section{examples.scrbl}
@include-section{implementation.scrbl}
@include-section{evaluation.scrbl}
@include-section{related.scrbl}
@include-section{conclusion.scrbl}
@include-section{documentation.scrbl}
