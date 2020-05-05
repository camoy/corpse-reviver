#lang typed/racket

;; Populations of Automata

(provide
  build-random-population
  population-payoffs
  match-up*
  death-birth
  ;; ==
  ;Payoff
  ;Population
)
 (: build-random-population
  ;; (build-population n c) for even n, build a population of size n
  ;; with c constraint: (even? n)
  (-> Natural Population))
 (: population-payoffs (-> Population [Listof Payoff]))
 (: match-up*
  ;; (match-ups p r) matches up neighboring pairs of
  ;; automata in population p for r rounds
  (-> Population Natural Population))
 (: death-birth
  ;; (death-birth p r) replaces r elements of p with r "children" of
  ;; randomly chosen fittest elements of p, also shuffle
  ;; constraint (< r (length p))
  (->* [Population Natural] [(U False Real)] Population))

;; =============================================================================
(require scv-cr/require-typed-check
 "automata-adapted.rkt")
(require/typed/check "utilities.rkt"
 (choose-randomly
  (->* [[Listof Probability] Natural] [(U False Real)] [Listof Natural]))
)

;; Population = (Cons Automaton* Automaton*)
;; Automaton* = [Vectorof Automaton]

(define DEF-COO 2)

;; -----------------------------------------------------------------------------
(define (build-random-population n)
  (define v (build-vector n (lambda (_) (make-random-automaton DEF-COO))))
  (cons v v))

;; -----------------------------------------------------------------------------
(define (population-payoffs population0)
  (define population (car population0))
  (for/list ([a population]) (automaton-payoff a)))

;; -----------------------------------------------------------------------------

(define (match-up* population0 rounds-per-match)
  (define a* (car population0))
  ;; comment out this line if you want cummulative payoff histories:
  ;; see below in birth-death
  (population-reset a*)
  ;; -- IN --
  (for ([i (in-range 0 (- (vector-length a*) 1) 2)])
    (define p1 (vector-ref a* i))
    (define p2 (vector-ref a* (+ i 1)))
    (define-values (a1 a2) (match-pair p1 p2 rounds-per-match))
    (vector-set! a* i a1)
    (vector-set! a* (+ i 1) a2))
  population0)

(: population-reset (-> Automaton* Void))
;; effec: reset all automata in a*
(define (population-reset a*)
  (for ([x (in-list (vector->list a*))][i (in-naturals)])
    (vector-set! a* i (automaton-reset x))))

;; -----------------------------------------------------------------------------

(define (death-birth population0 rate (q #false))
  (match-define (cons a* b*) population0)
  (define payoffs
    (for/list : [Listof Payoff] ([x : Automaton (in-list (vector->list a*))])
      (automaton-payoff x)))
  [define substitutes (choose-randomly payoffs rate q)]
  (for ([i (in-range rate)][p (in-list substitutes)])
    (vector-set! a* i (clone (vector-ref b* p))))
  (shuffle-vector a* b*))

(: shuffle-vector
   (All (X) (-> (Vectorof X) (Vectorof X) (cons (Vectorof X) (Vectorof X)))))
;; effect: shuffle vector b into vector a
;; constraint: (= (vector-length a) (vector-length b))
;; Fisher-Yates Shuffle

(define (shuffle-vector b a)
  ;; copy b into a
  (for ([x (in-list (vector->list b))][i (in-naturals)])
    (vector-set! a i x))
  ;; now shuffle a
  (for ([x (in-list (vector->list b))] [i (in-naturals)])
    (define j (random (add1 i)))
    (unless (= j i) (vector-set! a i (vector-ref a j)))
    (vector-set! a j x))
  (cons a b))
