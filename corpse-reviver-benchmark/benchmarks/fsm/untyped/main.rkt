#lang racket

;; Run a Simulation of Interacting Automata
(random-seed 7480)

;; =============================================================================
(require corpse-reviver/require-typed-check
 "untyped.rkt"
 "automata.rkt"
 "population.rkt"
 "utilities.rkt")

(define (payoff? x)
  (and (real? x) (<= 0 x)))

;; effect run timed simulation, create and display plot of average payoffs
;; effect measure time needed for the simulation
(define (main)
   (simulation->lines
    (evolve (build-random-population 300) 500 100 20))
   (void))

(define (simulation->lines data)
  (for/list 
    ([d  (in-list data)][n  (in-naturals)])
    (list n d)))

;; computes the list of average payoffs over the evolution of population p for
;; c cycles of of match-ups with r rounds per match and at birth/death rate of s
(define (evolve p c s r)
  (cond
    [(zero? c) '()]
    [else (define p2 (match-up* p r))
          ;; Note r is typed as State even though State is not exported 
          (define pp (population-payoffs p2))
          (define p3 (death-birth p2 s))
          ;; Note s same as r
          (cons
           (assert (relative-average pp r) payoff?)
           ;; Note evolve is assigned (-> ... [Listof Probability])
           ;; even though it is explicitly typed ... [Listof Payoff]
           (evolve p3 (- c 1) s r))]))

;; -----------------------------------------------------------------------------
(time (main))

