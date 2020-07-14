#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [rename -topological-sort topological-sort (->* (hash?) (list?) list?)]
  [hash-remove* (-> hash? list? hash?)]
  [on (parametric->/c [A B] (-> (-> A B) (-> B B boolean?) (-> A A boolean?)))]
  [satisfies (parametric->/c [A] (-> A (-> A boolean?) (or/c A #f)))]
  [path->symbol (-> path-string? symbol?)]
  [symbol->path (-> symbol? path-string?)]
  [proper-list? predicate/c]
  [flatten-cons (-> any/c set?)]
  [symbol->number (-> symbol? (or/c rational? #f))]
  [</id (-> symbol? symbol? boolean?)]
  [make-struct-names (-> symbol? (listof symbol?) (listof symbol?))]
  [lang-scv (-> syntax? symbol?)]
  [lang-opt (-> syntax? symbol?)]
  [relevant-target? (->* (path-string?) (boolean?) boolean?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require mischief/dict
         mischief/sort
         racket/function
         racket/list
         racket/match
         racket/path
         racket/set
         racket/string
         racket/syntax
         syntax/struct
         threading)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; [Hash A A] {[Listof A]} → [Listof A]
;; Topologically sort a list of nodes by a directed acyclic graph. If no nodes
;; are provided, all elements of the graph will be sorted.
(define (-topological-sort dag [nodes #f])
  (define nodes* (or nodes (hash-keys dag)))
  (define sorted
    (topological-sort
     nodes*
     (dict->procedure #:failure (const empty) dag)))
  (filter (λ~> (member nodes*)) sorted))

;; [Hash K V] [Listof K] → [Hash K V]
;; Removes every key in keys from the hash.
(define (hash-remove* this keys)
  (for/fold ([this this])
            ([key (in-list keys)])
    (hash-remove this key)))

;; [A → B] [B B → Boolean] → (A A → Boolean)
;; Orders elements according to the value of the element on f using <:.
(define ((on f <:) x y)
  (<: (f x) (f y)))

;; A (A → Boolean) → [Or A #f]
;; If the value satisfies the predicate, return it. Otherwise give back false.
(define (satisfies x predicate?)
  (and (predicate? x) x))

;; Path-String → Symbol
;; Converts a path to a symbol.
(define path->symbol
  (λ~>> simple-form-path
        path->string
        string->symbol))

;; Symbol → Path-String
;; Converts a symbol to a path.
(define symbol->path
  (λ~>> symbol->string
        string->path
        simple-form-path))

;; Any → Boolean
;; Returns if the input is a non-empty list.
(define (proper-list? xs)
  (and (list? xs) (not (empty? xs))))

;; Any → [Set Any]
;; Flattens a cons cell into a set of its components.
(define (flatten-cons x)
  (match x
    [#f           (set)]
    [(cons l r)   (set-union (flatten-cons l) (flatten-cons r))]
    [z            (set z)]))

;; Symbol → [Or Rational #f]
;; Transforms a contract definition identifier into a rational that is correctly
;; ordered according to contract-order.
(define (symbol->number id)
  (define id* (symbol->string id))
  (for/first ([(rx k) (in-indexed (in-list contract-order))]
              #:when (regexp-match? rx id*))
    (match-define (pregexp rx (list _ n)) id*)
    (- k (1/n n))))

;; Symbol Symbol → Boolean
;; Order on contract definition identifiers.
(define </id (symbol->number . on . <))

;; [Listof String]
;; Regular expressions for ordering contract definitions. We need to sort the
;; contracts in this specific order since, for example, a "generated-contract"
;; can depend on a "g" definition.
(define contract-order
  '(#px"^g(\\d+)$"
    #px"^l(\\d+)$"
    #px"^generated-contract(\\d+)$"))

;; String → Rational
;; Given a string denominator, calculate 1/n as a rational.
(define (1/n n) (/ 1 (string->number n)))

;; Symbol [Listof Symbol] → [Listof Symbol]
;; Return the name of possible exports from struct name and fields.
(define (make-struct-names name fields)
  (define-values (name* fields*)
    (values (datum->syntax #f name)
            (map (λ~>> (datum->syntax #f)) fields)))
  (define struct-names
    (build-struct-names name* fields* #:constructor-name name* #f #f))
  (map syntax->datum struct-names))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; language

;; Symbol → (Syntax → Symbol)
;; Returns the name of the no-check language for the new module.
(define ((make-lang kind) l)
  (define (format-lib lang)
    (format-symbol "corpse-reviver/private/lang/~a/~a" kind lang))
  (match (syntax-e l)
    ['racket/base (format-lib "untyped/base")]
    ['racket (format-lib "untyped/full")]
    ['typed/racket/base (format-lib "typed/base")]
    ['typed/racket (format-lib "typed/full")]
    [else (error 'make-lang "unknown language ~a" l)]))

;; Syntax → Symbol
;; Returns the language for analysis.
(define lang-scv (make-lang 'scv))

;; Syntax → Symbol
;; Returns the language for optimization.
(define lang-opt (make-lang 'opt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; path relevance

;; Path {Boolean} → Boolean
;; Returns if a target is relevant (i.e. is a Racket file and unless
;; no-skip? is #t, ignores _ prefixed files).
(define (relevant-target? target [no-skip? #f])
  (and (path-has-extension? target #".rkt")
       (or no-skip? (not (underscore-prefixed? target)))))

;; Path → Boolean
;; Returns if the filename at the given path has an underscore prefix.
(define (underscore-prefixed? target)
  (~> target file-name-from-path path->string (string-prefix? "_")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (with-chk (['name "topological-sort"])
    (chk
     (-topological-sort (hash 'a '(b) 'b '(c)) '(a b c))
     '(c b a)
     (-topological-sort (hash 'a '(b) 'b '(c)) '(a b))
     '(b a)
     (-topological-sort (hash 'a '(b) 'b '(c)))
     '(b a)))

  (with-chk (['name "hash-remove*"])
    (chk
     (hash-remove* (hash 'a 1 'b 2 'c 3) '(a b))
     (hash 'c 3)))

  (with-chk (['name "on"])
    (chk
     (sort '(1 2 3 4 5 6) (- . on . <))
     '(6 5 4 3 2 1)))

  (with-chk (['name "satisfies"])
    (chk
     #:! #:t (satisfies 1 even?)
     (satisfies 2 even?) 2))

  (with-chk (['name "symbol->path, path->symbol"])
    (chk
     (~> "." string->path path->symbol symbol->path) (current-directory)
     (~> '|.| symbol->path path->symbol symbol->path) (current-directory)))

  (with-chk (['name "proper-list?"])
    (chk
     #:! #:t (proper-list? '())
     #:t (proper-list? '(a))))

  (with-chk (['name "flatten-cons"])
    (chk
     (flatten-cons #f) (set)
     (flatten-cons (cons 1 2)) (set 1 2)
     (flatten-cons (cons (cons 1 2) 3)) (set 1 2 3)
     (flatten-cons 1) (set 1)))

  (with-chk (['name "symbol->number"])
    (chk
     #:t (symbol->number 'g5)
     #:t (symbol->number 'generated-contract5)
     #:t (symbol->number 'l17)
     #:! #:t (symbol->number 'blah)))

  (with-chk (['name "</id"])
    (chk
     #:t (</id 'g5 'g55)
     #:t (</id 'g55 'l7)
     #:! #:t (</id 'generated-contract5 'l7)
     #:t (</id 'l7 'generated-contract5)
     #:! #:t (</id 'generated-contract7 'generated-contract5)
     #:t (</id 'generated-contract1 'generated-contract13)))

  (with-chk (['name "1/n"])
    (chk
     (1/n "5") 1/5))

  (with-chk (['name "make-struct-names"])
    (chk
     (make-struct-names 'stream '(first rest))
     '(struct:stream
       stream
       stream?
       stream-first
       set-stream-first!
       stream-rest
       set-stream-rest!)))
  )