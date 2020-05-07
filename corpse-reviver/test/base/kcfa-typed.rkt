#lang typed/racket/base

;; Abstract Interpretation

(require
  "structs-adapted.rkt"
  "benv-adapted.rkt"
  "time-adapted.rkt"
  "denotable-adapted.rkt"
  racket/set
  (only-in racket/match match-define)
)

;; ---

(provide
  atom-eval
  next
  explore
)

;; =============================================================================

(: atom-eval (-> BEnv Store (-> Exp Denotable)))
(define ((atom-eval benv store) id)
  (cond
    [(Ref? id)
     (store-lookup store (benv-lookup benv (Ref-var id)))]
    [(Lam? id)
     (set (Closure id benv))]
    [else
     (error "atom-eval got a plain Exp")]))

(: next (-> State (Setof State)))
(define (next st)
  (match-define (State c benv store time) st)
  (cond
    [(Call? c)
     (define time* (tick c time))
     (match-define (Call _ f args) c)
     (: procs Denotable)
     (define procs ((atom-eval benv store) f))
     (: params (Listof Denotable))
     (define params (map (atom-eval benv store) args))
     (: new-states (Listof State))
     (define new-states
       (for/list ([proc : Value (in-set procs)])
         (match-define (Closure lam benv*) proc)
         (match-define (Lam _ formals call*) lam)
         (define bindings (map (alloc time*) formals))
         (define benv** (benv-extend* benv* formals bindings))
         (define store* (store-update* store bindings params))
         (State call* benv** store* time*)))
     (list->set new-states)]
    [else (set)]))

;; -- state space exploration

(: explore (-> (Setof State) (Listof State) (Setof State)))
(define (explore seen todo)
  (cond
    [(eq? '() todo)
     ;; Nothing left to do
     seen]
    [(set-member? seen (car todo))
     ;; Already seen current todo, move along
     (explore seen (cdr todo))]
    [else
      (define st0 (car todo))
      (: succs (Setof State))
      (define succs (next st0))
      (explore (set-add seen st0)
               (append (set->list succs) (cdr todo)))]))

#lang typed/racket/base

;; Binding environment,
;; helper functions

(require
  "structs-adapted.rkt"
)

(provide
  (struct-out Closure)
  (struct-out Binding)
  empty-benv
  benv-lookup
  benv-extend
  benv-extend*
)

;; =============================================================================

;; -- private

(define-type BEnv (HashTable Var Addr))
(define-type Addr Binding)
(define-type Time (Listof Label))

;; -- structs

(struct Closure
 ([lam : Lam]
  [benv : BEnv]))

(struct Binding
 ([var : Var]
  [time : Time]))

;; -- public

(: empty-benv BEnv)
(define empty-benv (make-immutable-hasheq '()))

(: benv-lookup (-> BEnv Var Addr))
(define benv-lookup hash-ref)

(: benv-extend (-> BEnv Var Addr BEnv))
(define benv-extend hash-set)

(: benv-extend* (-> BEnv (Listof Var) (Listof Addr) BEnv))
(define (benv-extend* benv vars addrs)
  (for/fold ([benv benv])
    ([v (in-list vars)]
     [a (in-list addrs)])
    (benv-extend benv v a)))

#lang typed/racket/base

;; Denotable values and stores to hold them.
;; A denotable is a set of values
;; (A value is a closure)

(require
  scv-cr/require-typed-check
  racket/set
  "structs-adapted.rkt"
  "benv-adapted.rkt"
  "time-adapted.rkt"
)

;; -----------------------------------------------------------------------------

(provide
  (struct-out State)
  d-bot
  d-join
  empty-store
  store-lookup
  store-update
  store-update*
  store-join
)

;; =============================================================================

;; -- private
(define-type Denotable (Setof Value))
(define-type Store (HashTable Addr Denotable))

;; -- structs

(struct State
 ([call : Exp]
  [benv : BEnv]
  [store : Store]
  [time : Time]))

;; -- public

(: d-bot Denotable)
(define d-bot (set))

(: d-join (-> Denotable Denotable Denotable))
(define d-join set-union)

(: empty-store Store)
(define empty-store (make-immutable-hasheq '()))

(: store-lookup (-> Store Addr Denotable))
(define (store-lookup s a)
  (hash-ref s a (lambda () d-bot)))

(: store-update (-> Store Addr Denotable Store))
(define (store-update store addr value)
  (: update-lam (-> Denotable Denotable))
  (define (update-lam d) (d-join d value))
  (hash-update store addr update-lam (lambda () d-bot)))

(: store-update* (-> Store (Listof Addr) (Listof Denotable) Store))
(define (store-update* s as vs)
  (for/fold ([store s])
    ([a (in-list as)]
     [v (in-list vs)])
    (store-update store a v)))

(: store-join (-> Store Store Store))
(define (store-join s1 s2)
  (for/fold ([new-store s1])
    ([(k v) (in-hash s2)])
    (store-update new-store k v)))

#lang typed/racket/base

;; Create a few examples and run abstract interpretation

(require
  scv-cr/require-typed-check
  "structs-adapted.rkt"
)
(require/typed/check "ui.rkt"
  [analyze (-> Exp MonoStore)]
  [format-mono-store (-> MonoStore String)])

;; =============================================================================
(define-type MonoStore (HashTable Var (Setof Exp)))

(define new-label gensym)

(: make-ref (-> Var Exp))
(define (make-ref var)
  (Ref (new-label) var))

(: make-lambda (-> (Listof Var) Exp Exp))
(define (make-lambda formals call)
  (Lam (new-label) formals call))

(: make-call (-> Exp Exp * Exp))
(define (make-call fun . args)
  (Call (new-label) fun args))

;; -- main

;; multiplication distributes over addition test: (2 * (1 + 3)) = ((2 * 1) + (2 * 3)
(define ianj (make-call (make-lambda '($f43) (make-call (make-lambda '($e44) (make-call (make-ref '$f43) (make-ref '$e44))) (make-lambda '(p1 $k45) (make-call (make-ref '$k45) (make-lambda '(p2 $k46) (make-call (make-ref '$k46) (make-lambda '(pf $k47) (make-call (make-ref '$k47) (make-lambda '(x $k48) (make-call (make-lambda '($f55) (make-call (make-lambda '($e56) (make-call (make-ref '$f55) (make-ref '$e56) (make-lambda '($f49) (make-call (make-lambda '($f53) (make-call (make-lambda '($e54) (make-call (make-ref '$f53) (make-ref '$e54) (make-lambda '($f51) (make-call (make-lambda '($e52) (make-call (make-ref '$f51) (make-ref '$e52) (make-lambda '($e50) (make-call (make-ref '$f49) (make-ref '$e50) (make-ref '$k48))))) (make-ref 'x))))) (make-ref 'pf))) (make-ref 'p2))))) (make-ref 'pf))) (make-ref 'p1))))))))))) (make-lambda '(plus $k57) (make-call (make-lambda '($f58) (make-call (make-lambda '($e59) (make-call (make-ref '$f58) (make-ref '$e59) (make-ref '$k57))) (make-lambda '(m1 $k60) (make-call (make-ref '$k60) (make-lambda '(m2 $k61) (make-call (make-ref '$k61) (make-lambda '(mf $k62) (make-call (make-lambda '($f63) (make-call (make-lambda '($f65) (make-call (make-lambda '($e66) (make-call (make-ref '$f65) (make-ref '$e66) (make-lambda '($e64) (make-call (make-ref '$f63) (make-ref '$e64) (make-ref '$k62))))) (make-ref 'mf))) (make-ref 'm1))) (make-ref 'm2))))))))) (make-lambda '(mult $k67) (make-call (make-lambda '($f68) (make-call (make-lambda '($e69) (make-call (make-ref '$f68) (make-ref '$e69) (make-ref '$k67))) (make-lambda '(n $k70) (make-call (make-ref '$k70) (make-lambda '(rf $k71) (make-call (make-ref '$k71) (make-lambda '(rx $k72) (make-call (make-lambda '($f79) (make-call (make-lambda '($e80) (make-call (make-ref '$f79) (make-ref '$e80) (make-lambda '($f76) (make-call (make-lambda '($e77) (make-call (make-ref '$f76) (make-ref '$e77) (make-lambda '($f73) (make-call (make-lambda '($e74) (make-call (make-ref '$f73) (make-ref '$e74) (make-ref '$k72))) (make-lambda '(id $k75) (make-call (make-ref '$k75) (make-ref 'id))))))) (make-lambda '(ignored $k78) (make-call (make-ref '$k78) (make-ref 'rx))))))) (make-lambda '(g $k81) (make-call (make-ref '$k81) (make-lambda '(h $k82) (make-call (make-lambda '($f83) (make-call (make-lambda '($f85) (make-call (make-lambda '($e86) (make-call (make-ref '$f85) (make-ref '$e86) (make-lambda '($e84) (make-call (make-ref '$f83) (make-ref '$e84) (make-ref '$k82))))) (make-ref 'rf))) (make-ref 'g))) (make-ref 'h))))))) (make-ref 'n))))))))) (make-lambda '(pred $k87) (make-call (make-lambda '($f88) (make-call (make-lambda '($e89) (make-call (make-ref '$f88) (make-ref '$e89) (make-ref '$k87))) (make-lambda '(s1 $k90) (make-call (make-ref '$k90) (make-lambda '(s2 $k91) (make-call (make-lambda '($f94) (make-call (make-lambda '($e95) (make-call (make-ref '$f94) (make-ref '$e95) (make-lambda '($f92) (make-call (make-lambda '($e93) (make-call (make-ref '$f92) (make-ref '$e93) (make-ref '$k91))) (make-ref 's1))))) (make-ref 'pred))) (make-ref 's2))))))) (make-lambda '(sub $k96) (make-call (make-lambda '($f97) (make-call (make-lambda '($e98) (make-call (make-ref '$f97) (make-ref '$e98) (make-ref '$k96))) (make-lambda '(f0 $k99) (make-call (make-ref '$k99) (make-lambda '(x0 $k100) (make-call (make-ref '$k100) (make-ref 'x0))))))) (make-lambda '(church0 $k101) (make-call (make-lambda '($f102) (make-call (make-lambda '($e103) (make-call (make-ref '$f102) (make-ref '$e103) (make-ref '$k101))) (make-lambda '(f1 $k104) (make-call (make-ref '$k104) (make-lambda '(x1 $k105) (make-call (make-lambda '($f106) (make-call (make-lambda '($e107) (make-call (make-ref '$f106) (make-ref '$e107) (make-ref '$k105))) (make-ref 'x1))) (make-ref 'f1))))))) (make-lambda '(church1 $k108) (make-call (make-lambda '($f109) (make-call (make-lambda '($e110) (make-call (make-ref '$f109) (make-ref '$e110) (make-ref '$k108))) (make-lambda '(f2 $k111) (make-call (make-ref '$k111) (make-lambda '(x2 $k112) (make-call (make-lambda '($f113) (make-call (make-lambda '($f115) (make-call (make-lambda '($e116) (make-call (make-ref '$f115) (make-ref '$e116) (make-lambda '($e114) (make-call (make-ref '$f113) (make-ref '$e114) (make-ref '$k112))))) (make-ref 'x2))) (make-ref 'f2))) (make-ref 'f2))))))) (make-lambda '(church2 $k117) (make-call (make-lambda '($f118) (make-call (make-lambda '($e119) (make-call (make-ref '$f118) (make-ref '$e119) (make-ref '$k117))) (make-lambda '(f3 $k120) (make-call (make-ref '$k120) (make-lambda '(x3 $k121) (make-call (make-lambda '($f122) (make-call (make-lambda '($f124) (make-call (make-lambda '($f126) (make-call (make-lambda '($e127) (make-call (make-ref '$f126) (make-ref '$e127) (make-lambda '($e125) (make-call (make-ref '$f124) (make-ref '$e125) (make-lambda '($e123) (make-call (make-ref '$f122) (make-ref '$e123) (make-ref '$k121))))))) (make-ref 'x3))) (make-ref 'f3))) (make-ref 'f3))) (make-ref 'f3))))))) (make-lambda '(church3 $k128) (make-call (make-lambda '($f129) (make-call (make-lambda '($e130) (make-call (make-ref '$f129) (make-ref '$e130) (make-ref '$k128))) (make-lambda '(ta $k131) (make-call (make-ref '$k131) (make-lambda '(tb $k132) (make-call (make-lambda '($f133) (make-call (make-lambda '($e134) (make-call (make-ref '$f133) (make-ref '$e134) (make-ref '$k132))) (make-lambda '(adummy $k135) (make-call (make-ref '$k135) (make-ref 'adummy))))) (make-ref 'ta))))))) (make-lambda '(true $k136) (make-call (make-lambda '($f137) (make-call (make-lambda '($e138) (make-call (make-ref '$f137) (make-ref '$e138) (make-ref '$k136))) (make-lambda '(fa $k139) (make-call (make-ref '$k139) (make-lambda '(fb $k140) (make-call (make-lambda '($f141) (make-call (make-lambda '($e142) (make-call (make-ref '$f141) (make-ref '$e142) (make-ref '$k140))) (make-lambda '(bdummy $k143) (make-call (make-ref '$k143) (make-ref 'bdummy))))) (make-ref 'fb))))))) (make-lambda '(false $k144) (make-call (make-lambda '($f145) (make-call (make-lambda '($e146) (make-call (make-ref '$f145) (make-ref '$e146) (make-ref '$k144))) (make-lambda '(z $k147) (make-call (make-lambda '($f150) (make-call (make-lambda '($e151) (make-call (make-ref '$f150) (make-ref '$e151) (make-lambda '($f148) (make-call (make-lambda '($e149) (make-call (make-ref '$f148) (make-ref '$e149) (make-ref '$k147))) (make-ref 'true))))) (make-lambda '(zx $k152) (make-call (make-ref '$k152) (make-ref 'false))))) (make-ref 'z))))) (make-lambda '(church0? $k153) (make-call (make-lambda '($f154) (make-call (make-lambda '($e155) (make-call (make-ref '$f154) (make-ref '$e155) (make-ref '$k153))) (make-lambda '(yf $k156) (make-call (make-lambda '($f157) (make-call (make-lambda '($e158) (make-call (make-ref '$f157) (make-ref '$e158) (make-ref '$k156))) (make-lambda '(yx $k159) (make-call (make-lambda '($f160) (make-call (make-lambda '($e161) (make-call (make-ref '$f160) (make-ref '$e161) (make-ref '$k159))) (make-lambda '(yv $k162) (make-call (make-lambda '($f165) (make-call (make-lambda '($e166) (make-call (make-ref '$f165) (make-ref '$e166) (make-lambda '($f163) (make-call (make-lambda '($e164) (make-call (make-ref '$f163) (make-ref '$e164) (make-ref '$k162))) (make-ref 'yv))))) (make-ref 'yx))) (make-ref 'yx))))) (make-ref 'yf))))) (make-lambda '(yg $k167) (make-call (make-lambda '($f168) (make-call (make-lambda '($e169) (make-call (make-ref '$f168) (make-ref '$e169) (make-ref '$k167))) (make-ref 'yg))) (make-ref 'yg))))))) (make-lambda '(Y $k170) (make-call (make-lambda '($f171) (make-call (make-lambda '($f173) (make-call (make-lambda '($e174) (make-call (make-ref '$f173) (make-ref '$e174) (make-lambda '($e172) (make-call (make-ref '$f171) (make-ref '$e172) (make-ref '$k170))))) (make-lambda '(church=? $k175) (make-call (make-ref '$k175) (make-lambda '(e1 $k176) (make-call (make-ref '$k176) (make-lambda '(e2 $k177) (make-call (make-lambda '($f205) (make-call (make-lambda '($e206) (make-call (make-ref '$f205) (make-ref '$e206) (make-lambda '($f200) (make-call (make-lambda '($e201) (make-call (make-ref '$f200) (make-ref '$e201) (make-lambda '($f178) (make-call (make-lambda '($e179) (make-call (make-ref '$f178) (make-ref '$e179) (make-ref '$k177))) (make-lambda '(elsedummy1 $k180) (make-call (make-lambda '($f198) (make-call (make-lambda '($e199) (make-call (make-ref '$f198) (make-ref '$e199) (make-lambda '($f196) (make-call (make-lambda '($e197) (make-call (make-ref '$f196) (make-ref '$e197) (make-lambda '($f181) (make-call (make-lambda '($e182) (make-call (make-ref '$f181) (make-ref '$e182) (make-ref '$k180))) (make-lambda '(elsedummy2 $k183) (make-call (make-lambda '($f190) (make-call (make-lambda '($f194) (make-call (make-lambda '($e195) (make-call (make-ref '$f194) (make-ref '$e195) (make-lambda '($f192) (make-call (make-lambda '($e193) (make-call (make-ref '$f192) (make-ref '$e193) (make-lambda '($e191) (make-call (make-ref '$f190) (make-ref '$e191) (make-lambda '($f184) (make-call (make-lambda '($f188) (make-call (make-lambda '($e189) (make-call (make-ref '$f188) (make-ref '$e189) (make-lambda '($f186) (make-call (make-lambda '($e187) (make-call (make-ref '$f186) (make-ref '$e187) (make-lambda '($e185) (make-call (make-ref '$f184) (make-ref '$e185) (make-ref '$k183))))) (make-ref 'church1))))) (make-ref 'e2))) (make-ref 'sub))))))) (make-ref 'church1))))) (make-ref 'e1))) (make-ref 'sub))) (make-ref 'church=?))))))) (make-ref 'false))))) (make-ref 'e2))) (make-ref 'church0?))))))) (make-lambda '(thendummy $k202) (make-call (make-lambda '($f203) (make-call (make-lambda '($e204) (make-call (make-ref '$f203) (make-ref '$e204) (make-ref '$k202))) (make-ref 'e2))) (make-ref 'church0?))))))) (make-ref 'e1))) (make-ref 'church0?))))))))) (make-ref 'Y))) (make-lambda '(church=? $k207) (make-call (make-lambda '($f222) (make-call (make-lambda '($f230) (make-call (make-lambda '($e231) (make-call (make-ref '$f230) (make-ref '$e231) (make-lambda '($f224) (make-call (make-lambda '($f228) (make-call (make-lambda '($e229) (make-call (make-ref '$f228) (make-ref '$e229) (make-lambda '($f226) (make-call (make-lambda '($e227) (make-call (make-ref '$f226) (make-ref '$e227) (make-lambda '($e225) (make-call (make-ref '$f224) (make-ref '$e225) (make-lambda '($e223) (make-call (make-ref '$f222) (make-ref '$e223) (make-lambda '($f208) (make-call (make-lambda '($f216) (make-call (make-lambda '($f220) (make-call (make-lambda '($e221) (make-call (make-ref '$f220) (make-ref '$e221) (make-lambda '($f218) (make-call (make-lambda '($e219) (make-call (make-ref '$f218) (make-ref '$e219) (make-lambda '($e217) (make-call (make-ref '$f216) (make-ref '$e217) (make-lambda '($f210) (make-call (make-lambda '($f214) (make-call (make-lambda '($e215) (make-call (make-ref '$f214) (make-ref '$e215) (make-lambda '($f212) (make-call (make-lambda '($e213) (make-call (make-ref '$f212) (make-ref '$e213) (make-lambda '($e211) (make-call (make-ref '$f210) (make-ref '$e211) (make-lambda '($e209) (make-call (make-ref '$f208) (make-ref '$e209) (make-ref '$k207))))))) (make-ref 'church3))))) (make-ref 'church2))) (make-ref 'mult))))))) (make-ref 'church1))))) (make-ref 'church2))) (make-ref 'mult))) (make-ref 'plus))))))))) (make-ref 'church3))))) (make-ref 'church1))) (make-ref 'plus))))) (make-ref 'church2))) (make-ref 'mult))) (make-ref 'church=?)))))))))))))))))))))))))))))

(: main (-> Natural Void))
(define (main N)
  (for ([a-k (in-range N)])
    (analyze ianj)))

(time (main 1)) ;; 20 seconds
;;(time (main 2)) ;; 40 seconds
#lang typed/racket/base

(provide
  (struct-out Stx)
  (struct-out exp)
  (struct-out Ref)
  (struct-out Lam)
  (struct-out Call)
)

;; =============================================================================

(struct Stx
 ([label : Symbol]))

(struct exp Stx ())

(struct Ref exp
 ([var : Symbol]))

(struct Lam exp
 ([formals : (Listof Symbol)]
  [call : (U exp Ref Lam Call)]))

(struct Call Stx
 ([fun : (U exp Ref Lam Call)]
  [args : (Listof (U exp Ref Lam Call))]))

#lang typed/racket/base

(require
  "structs-adapted.rkt"
  "benv-adapted.rkt"
)

;; ---

(provide
  time-zero
  k
  tick
  alloc
)

;; =============================================================================

;; ---
(define-type Value Closure)

(: take* (All (A) (-> (Listof A) Natural (Listof A))))
(define (take* l n)
  (for/list ([e (in-list l)]
             [i (in-range n)])
    e))

;; ---

(: time-zero Time)
(define time-zero '())

(: k (Parameterof Natural))
(define k (make-parameter 1))

(: tick (-> Stx Time Time))
(define (tick call time)
  (define label (Stx-label call))
  (take* (cons label time) (k)))

(: alloc (-> Time (-> Var Addr)))
(define ((alloc time) var)
  (Binding var time))

#lang typed/racket/base

;; User Interface to `ai.rkt`

(require
  scv-cr/require-typed-check
  racket/set
  "structs-adapted.rkt"
  "benv-adapted.rkt"
  "denotable-adapted.rkt"
  "time-adapted.rkt"
  (only-in racket/string string-join)
)

(require/typed/check "ai.rkt"
  (atom-eval (-> BEnv Store (-> Exp Denotable)))
  (next (-> State (Setof State)))
  (explore (-> (Setof State) (Listof State) (Setof State)))
)
;; ---

(provide
  summarize
  empty-mono-store
  monovariant-value
  monovariant-store
  analyze
  format-mono-store
)

;; =============================================================================

;; -- ui.rkt
(define-type MonoStore (HashTable Var (Setof Exp)))

(: summarize (-> (Setof State) Store))
(define (summarize states)
  (for/fold ([store empty-store])
    ([state (in-set states)])
    (store-join (State-store state) store)))

(: empty-mono-store MonoStore)
(define empty-mono-store (make-immutable-hasheq '()))

(: monovariant-value (-> Value Lam))
(define (monovariant-value v)
  (Closure-lam v))

(: monovariant-store (-> Store MonoStore))
(define (monovariant-store store)
  (: update-lam (-> (Setof Value) (-> (Setof Exp) (Setof Exp))))
  (define ((update-lam vs) b-vs)
    (: v-vs (Setof Lam))
    (define v-vs (list->set (set-map vs monovariant-value)))
    (set-union b-vs v-vs))
  (: default-lam (-> (Setof Exp)))
  (define (default-lam) (set))
  (for/fold ([mono-store empty-mono-store])
    ([(b vs) (in-hash store)])
    (hash-update mono-store
                 (Binding-var b)
                 (update-lam vs)
                 default-lam)))

(: analyze (-> Exp MonoStore))
(define (analyze exp)
  (define init-state (State exp empty-benv empty-store time-zero))
  (define states (explore (set) (list init-state)))
  (define summary (summarize states))
  (define mono-store (monovariant-store summary))
  mono-store)

(: format-mono-store (-> MonoStore String))
(define (format-mono-store ms)
  (: res (Listof String))
  (define res
    (for/list ([(i vs) (in-hash ms)])
      (format "~a:\n~a"
              i
              (string-join
                (for/list : (Listof String) ([v (in-set vs)])
                  (format "\t~S" v))
                "\n"))))
  (string-join res "\n"))

