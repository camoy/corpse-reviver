#lang typed/racket

(define-type Color Symbol)
(require corpse-reviver/require-typed-check)
(require/typed/check "data.rkt"
  [#:struct posn ([x : Real]
                  [y : Real])]
  [#:struct block ([x : Real]
                   [y : Real]
                   [color : Color])]
  [#:struct tetra ([center : posn]
                   [blocks : (Listof Block)])]
  [#:struct world ([tetra : tetra]
                   [blocks : (Listof Block)])])

(define-type Posn posn)
(define-type Block block)
(define-type Tetra tetra)
(define-type World world)
(define-type BSet  (Listof Block))

(provide
 (struct-out posn)
 (struct-out block)
 (struct-out tetra)
 (struct-out world)
 Posn
 Block
 Tetra
 World
 Color
 BSet
 Color
 BSet)
