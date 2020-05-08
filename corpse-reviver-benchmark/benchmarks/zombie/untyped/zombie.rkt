#lang racket/base

(provide
 w0
 world-on-mouse
 world-on-tick
)

(require
  (for-syntax racket/sequence racket/base syntax/parse racket/syntax))

(require (only-in "image.rkt"
  empty-scene
  place-image
  circle
))
(require (only-in "math.rkt"
  min
  max
  abs
  sqr
  msqrt
))

;; =============================================================================

(define WIDTH 400)
(define HEIGHT 400)
(define MT-SCENE (empty-scene WIDTH HEIGHT))
(define PLAYER-SPEED 4)
(define ZOMBIE-SPEED 2)
(define ZOMBIE-RADIUS 20)
(define PLAYER-RADIUS 20)
(define PLAYER-IMG (circle PLAYER-RADIUS "solid" "green"))

;; Create an object type (-> Symbol (U (Pairof Symbol ?) ...))
;;  and getters for each member of the codomain union
(define-syntax make-fake-object-type*
  ;;bg; the untyped version ignores all types
  (syntax-parser
   [(_ ty [f* t*] ...)
    #:with (id* ...) (for/list ([f (in-list (syntax->list #'(f* ...)))])
                       (format-id #'ty "~a-~a" (string-downcase (symbol->string (syntax-e #'ty))) f))
    #:with (f-sym* ...) (for/list ([f (in-list (syntax->list #'(f* ...)))]) (syntax-e f))
    #'(begin
        ;(define-type ty (-> Symbol (U (Pairof 'f-sym* t*) ...)))
        (begin
          ;(: id* (-> ty t*))
          (define (id* v)
            (let ([r (v 'f-sym*)])
              (if (eq? 'f-sym* (car r))
                (cdr r)
                (error 'id* "type error"))))) ...)]))

;(define posn/c
; (->i ([msg (one-of/c 'x 'y 'posn 'move-toward/speed 'draw-on/image 'dist)])
;  (res (msg)
;   (cond
;    [(equal? msg 'x) (-> number?)]
;    [(equal? msg 'y) (-> number?)]
;    [(equal? msg 'posn) (-> posn/c)]
;    [(equal? msg 'move-toward/speed) (posn/c number? . -> . posn/c)]
;    [(equal? msg 'draw-on/image) (image? image? . -> . image?)]
;    [(equal? msg 'dist) (posn/c . -> . number?)]
;    [else "error"]))))

(make-fake-object-type* Posn
  [x (-> Real)]
  [y (-> Real)]
  [posn (-> Posn)]
  [move-toward/speed (-> Posn Real Posn)]
  [move (-> Real Real Posn)]
  [draw-on/image (-> Image Image Image)]
  [dist (-> Posn Real)])

;(define player/c
; (->i ([msg (one-of/c 'posn 'move-toward 'draw-on)])
;  (res (msg)
;   (cond
;    [(equal? msg 'posn) (-> posn/c)]
;    [(equal? msg 'move-toward) (posn/c . -> . player/c)]
;    [(equal? msg 'draw-on) (image? . -> . image?)]
;    [else "error"]))))

(make-fake-object-type* Player
  (posn (-> Posn))
  (move-toward (-> Posn Player))
  (draw-on (-> Image Image)))

;(define zombie/c
; (->i ([msg (one-of/c 'posn 'draw-on/color 'touching? 'move-toward)])
;  (res (msg)
;   (cond
;    [(equal? msg 'posn) (-> posn/c)]
;    [(equal? msg 'draw-on/color) (string? image? . -> . image?)]
;    [(equal? msg 'touching?) (posn/c . -> . boolean?)]
;    [(equal? msg 'move-toward) (posn/c . -> . zombie/c)]
;    [else "error"]))))

(make-fake-object-type* Zombie
  (posn (-> Posn))
  (draw-on/color (-> String Image Image))
  (touching? (-> Posn Boolean))
  (move-toward (-> Posn Zombie)))

;(define horde/c
; (->i ([msg (one-of/c 'dead 'undead 'draw-on 'touching? 'move-toward 'eat-brains)])
;  (res (msg)
;   (cond
;    [(equal? msg 'dead) (-> zombies/c)]
;    [(equal? msg 'undead) (-> zombies/c)]
;    [(equal? msg 'draw-on) (image? . -> . image?)]
;    [(equal? msg 'touching?) (posn/c . -> . boolean?)]
;    [(equal? msg 'move-toward) (posn/c . -> . horde/c)]
;    [(equal? msg 'eat-brains) (-> horde/c)]
;    [else "error"]))))

(make-fake-object-type* Horde
  (dead (-> Zombies))
  (undead (-> Zombies))
  (draw-on (-> Image Image))
  (touching? (-> Posn Boolean))
  (move-toward (-> Posn Horde))
  (eat-brains (-> Horde)))

;(define zombies/c
; (->i ([msg (one-of/c 'move-toward 'draw-on/color 'touching? 'kill-all)])
;  (res (msg)
;   (cond
;    [(equal? msg 'move-toward) (posn/c . -> . zombies/c)]
;    [(equal? msg 'draw-on/color) (string? image? . -> . image?)]
;    [(equal? msg 'touching?) (posn/c . -> . boolean?)]
;    [(equal? msg 'kill-all) (zombies/c . -> . horde/c)]
;    [else "error"]))))

(make-fake-object-type* Zombies
  (move-toward (-> Posn Zombies))
  (draw-on/color (-> String Image Image))
  (touching? (-> Posn Boolean))
  (kill-all (-> Zombies Horde)))

;(define world/c
; (->i ([msg (one-of/c 'on-mouse 'on-tick 'to-draw 'stop-when)])
;  (res (msg)
;   (cond
;    [(equal? msg 'on-mouse) (number? number? string? . -> . world/c)]
;    [(equal? msg 'on-tick) (-> world/c)]
;    [(equal? msg 'to-draw) (-> image?)]
;    [(equal? msg 'stop-when) (-> boolean?)]
;    [else "error"]))))

(make-fake-object-type* World
  (on-mouse (-> Real Real String World))
  (on-tick (-> World))
  (to-draw (-> Image))
  (stop-when (-> Boolean)))

(define (new-world player mouse zombies)
 (lambda (msg)
  (cond
   [(equal? msg 'on-mouse)
   (cons 'on-mouse
   (lambda (x y me)
    (new-world player
     (if (equal? me "leave") ((player-posn player)) (new-posn x y))
     zombies)))]
   [(equal? msg 'on-tick)
   (cons 'on-tick
    (lambda ()
    (new-world ((player-move-toward player) mouse)
     mouse
     ((horde-move-toward ((horde-eat-brains zombies))) ((player-posn player))))))]
   [(equal? msg 'to-draw)
   (cons 'to-draw
   (lambda ()
    ((player-draw-on player) ((horde-draw-on zombies) MT-SCENE))))]
   [(equal? msg 'stop-when)
   (cons 'stop-when
   (lambda ()
    ((horde-touching? zombies) ((player-posn player)))))]
   [else (error 'world "unknown message")])))

(define (new-player p)
  (lambda (msg)
   (cond
    [(equal? msg 'posn) (cons 'posn (lambda () p))]
    [(equal? msg 'move-toward)
    (cons 'move-toward
     (lambda (q)
     (new-player ((posn-move-toward/speed p) q PLAYER-SPEED))))]
    [(equal? msg 'draw-on)
    (cons 'draw-on
     (lambda (scn)
     ((posn-draw-on/image p) PLAYER-IMG scn)))]
    [else (error 'player "unknown message")])))

(define (new-horde undead dead)
 (lambda (msg)
  (cond
   [(equal? msg 'dead) (cons 'dead (lambda () dead))]
   [(equal? msg 'undead) (cons 'undead (lambda () undead))]
   [(equal? msg 'draw-on)
   (cons 'draw-on
     (lambda (scn)
    ((zombies-draw-on/color undead) "yellow" ((zombies-draw-on/color dead) "black" scn))))]
   [(equal? msg 'touching?)
   (cons 'touching?
    (lambda (p)
    (or ((zombies-touching? undead) p) ((zombies-touching? dead) p))))]
   [(equal? msg 'move-toward)
   (cons 'move-toward
    (lambda (p)
    (new-horde ((zombies-move-toward undead) p) dead)))]
   [(equal? msg 'eat-brains)
    (cons 'eat-brains
     (lambda () ((zombies-kill-all undead) dead)))]
   [else (error 'horde "unknown message")])))

(define (new-cons-zombies z r)
 (lambda (msg)
  (cond
   [(equal? msg 'move-toward)
   (cons 'move-toward
   (lambda (p)
    (new-cons-zombies ((zombie-move-toward z) p) ((zombies-move-toward r) p))))]
   [(equal? msg 'draw-on/color)
   (cons 'draw-on/color
    (lambda (c s)
    ((zombie-draw-on/color z) c ((zombies-draw-on/color r) c s))))]
   [(equal? msg 'touching?)
   (cons 'touching?
    (lambda (p)
    (or ((zombie-touching? z) p) ((zombies-touching? r) p))))]
   [(equal? msg 'kill-all)
   (cons 'kill-all
   (lambda (dead)
    (cond
     [(or ((zombies-touching? r) ((zombie-posn z)))
         ((zombies-touching? dead) ((zombie-posn z))))
     ((zombies-kill-all r) (new-cons-zombies z dead))]
     [else (let ([res ((zombies-kill-all r) dead)])
         (new-horde
          (new-cons-zombies z ((horde-undead res)))
          ((horde-dead res))))])))]
   [else (error 'zombies "unknown message")])))

(define (new-mt-zombies)
 (lambda (msg)
  (cond
   [(equal? msg 'move-toward) (cons 'move-toward (lambda (p) (new-mt-zombies)))]
   [(equal? msg 'draw-on/color) (cons 'draw-on/color (lambda (c s) s))]
   [(equal? msg 'touching?) (cons 'touching? (lambda (p) #f))]
   [(equal? msg 'kill-all)
   (cons 'kill-all
    (lambda (dead)
    (new-horde (new-mt-zombies) dead)))]
   [else (error 'zombies "unknown message")])))

(define (new-zombie p)
 (lambda (msg)
  (cond
   [(equal? msg 'posn) (cons 'posn (lambda () p))]
   [(equal? msg 'draw-on/color)
   (cons 'draw-on/color
   (lambda (c s)
    ((posn-draw-on/image p)
     (circle ZOMBIE-RADIUS "solid" c)
     s)))]
   [(equal? msg 'touching?)
   (cons 'touching?
   (lambda (q)
    (<= ((posn-dist p) q) ZOMBIE-RADIUS)))]
   [(equal? msg 'move-toward)
   (cons 'move-toward
    (lambda (q)
    (new-zombie ((posn-move-toward/speed p) q ZOMBIE-SPEED))))]
   [else (error 'zombie "unknown message")])))

(define (new-posn x y)
     (lambda (msg)
      (let ([this (new-posn x y)]) ; FIXME
       (cond
        [(equal? msg 'x) (cons 'x (lambda () x))]
        [(equal? msg 'y) (cons 'y (lambda () y))]
        [(equal? msg 'posn) (cons 'posn (lambda () this))]
        [(equal? msg 'move-toward/speed)
        (cons msg
        (lambda (p speed)
         (let* ([x2 (- ((posn-x p)) x)]
                [y2 (- ((posn-y p)) y)]
                [move-distance (min speed (max (abs x2) (abs y2)))])
          (cond
           [(< (abs x2) (abs y2))
           ((posn-move this)
            0
            (if (positive? y2) move-distance (- 0 move-distance)))]
           [else
           ((posn-move this)
            (if (positive? x2) move-distance (- 0 move-distance))
            0)]))))]
        [(equal? msg 'move)
        (cons 'move
         (lambda (x2 y2)
         (new-posn (+ x x2) (+ y y2))))]
        [(equal? msg 'draw-on/image)
         (cons 'draw-on/image
          (lambda (img scn)
           (place-image img x y scn)))]
        [(equal? msg 'dist)
         (cons 'dist
           (lambda (p)
            (msqrt (+ (sqr (- ((posn-y p)) y))
                   (sqr (- ((posn-x p)) x))))))]
        [else (error 'posn "unknown message")]))))

(define w0
 (new-world
  (new-player (new-posn 0 0))
  (new-posn 0 0)
  (new-horde
   (new-cons-zombies
    (new-zombie (new-posn 100 300))
    (new-cons-zombies
     (new-zombie (new-posn 100 200))
     (new-mt-zombies)))
   (new-cons-zombies
    (new-zombie (new-posn 200 200))
    (new-mt-zombies)))))

