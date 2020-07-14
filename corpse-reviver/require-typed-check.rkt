#lang racket/base

(provide
  require/typed/check
  ;; Same syntax as require/typed, but does not install contracts
  ;;  if the current module and providing module are typed.
)

(require
  (for-syntax racket/base syntax/location syntax/parse)
  (rename-in typed/racket/no-check [require/typed require/typed/no-check]))

;; =============================================================================


(define-for-syntax disable-require-typed-check?
  (and (getenv "DISABLE_REQUIRE_TYPED_CHECK") #true))

;; Q. Is the typed-lib cache worth using?
;;
;; Here are some timings from `test/` files, collected by running
;;  `rm -r test/compiled; time raco make -v test/TEST.rkt`
;; and eyeballing an average. (Times are in seconds.)
;;
;;   ___mod_|_stress_|_fsm_
;;   nohash |  3.00  | 6.46
;;     hash |  2.75  | 6.49
;;
;; So let's just keep the cache

;; typed-lib? : (Syntaxof Module-Path) -> Boolean
(define-for-syntax typed-lib?
  (let ([cache (make-hash)])
    (λ (lib-stx)
      (let* ([lib (syntax->datum lib-stx)]
             [dyn-path (and (not (relative-submod? lib))
                            (append
                              (if (submod? lib)
                                (resolve-submod #f #;(syntax-source-file-name lib-stx) lib)
                                (list 'submod lib))
                              '(#%type-decl)))])
        (hash-ref! cache lib
          (λ () ;; Typed Racket always installs a `#%type-decl` submodule
            (and dyn-path
                 (module-declared? dyn-path #true)
                 #true)))))))

;; : Require-Spec -> Boolean
(define-for-syntax (submod? x)
  (and (list? x) (not (null? x)) (eq? 'submod (car x))))

;; : Require-Spec -> Boolean
(define-for-syntax (relative-submod? x)
  (and (submod? x)
       (not (null? (cdr x)))
       (or (string=? "." (cadr x))
           (string=? ".." (cadr x)))))

;; : Submod-Path -> Module-Path
(define-for-syntax (resolve-submod src l)
  (case (cadr l)
   [(".." ".")
    ;; Circular dependency issue ... cannot compile the module without
    ;;  compiling the module's submodules.
    (raise-argument-error 'require/typed/check "Non-relative submodule" l)]
   [else
    l]))

;; -----------------------------------------------------------------------------
(begin-for-syntax
  ;; This block is copied from:
  ;;    https://github.com/racket/typed-racket/blob/master/typed-racket-lib/typed/private/no-check-helper.rkt#L17
  ;; We use it to parse clauses for no-check.
  ;; Except, I added the `ann` properties

  (define-syntax-class opt-parent
    #:attributes (nm parent)
    (pattern nm:id #:with parent #'#f)
    (pattern (nm:id parent:id)))

  (define-syntax-class opt-rename
    #:attributes (nm orig-nm spec)
    (pattern nm:id
      #:with orig-nm #'nm
      #:with spec #'nm)
    (pattern (orig-nm:id nm:id)
      #:with spec #'(orig-nm internal-nm)))

  (define-syntax-class simple-clause
    #:attributes (nm ty)
    (pattern [spec:opt-rename ty]
     #:attr nm #'spec.nm))

  (define-splicing-syntax-class (struct-opts struct-name)
    #:attributes (ctor-value type)
    (pattern (~seq (~optional (~seq (~and key (~or #:extra-constructor-name #:constructor-name))
                              name:id))
                   (~optional (~seq #:type-name type:id) #:defaults ([type struct-name])))
      #:attr ctor-value (if (attribute key) #'(key name) #'())))

  (define-syntax-class struct-clause
    #:attributes (nm type (body 1) (constructor-parts 1) (tvar 1))
    (pattern [(~or (~datum struct) #:struct)
              (~optional (~seq (tvar ...)) #:defaults ([(tvar 1) '()]))
              nm:opt-parent (body ...)
              (~var opts (struct-opts #'nm.nm))]
      #:with (constructor-parts ...) #'opts.ctor-value
      #:attr type #'opts.type))

  (define-syntax-class signature-clause
    #:literals (:)
    #:attributes (sig-name [var 1] [type 1])
    (pattern [#:signature sig-name:id ([var:id : type] ...)]))

  (define-syntax-class opaque-clause
    #:attributes (ty pred opt)
    (pattern [(~or (~datum opaque) #:opaque) ty:id pred:id]
      #:with opt #'())
    (pattern [(~or (~datum opaque) #:opaque) opaque ty:id pred:id #:name-exists]
      #:with opt #'(#:name-exists)))

  (define-syntax-class (clause lib)
    #:attributes (ann req)
      (pattern oc:opaque-clause
        #:attr ann #'#f
        #:attr req #`(#,(datum->syntax lib 'require/typed) #,lib (#:opaque oc.ty oc.pred . oc.opt)))
      (pattern (~var strc struct-clause)
        #:attr ann #'#f
        #:attr req #'#f)
        ; TODO check struct annotations
        ;#:attr spec
        ;#`(require-typed-struct strc.nm (strc.tvar ...)
        ;    (strc.body ...) strc.constructor-parts ...
        ;    #:type-name strc.type
        ;    #,@(if unsafe? #'(unsafe-kw) #'())
        ;    #,lib)
      (pattern sig:signature-clause
        #:attr ann #'#f
        #:attr req #'#f)
        ; TODO check signature annotations
      (pattern sc:simple-clause
        #:attr ann #'(ann sc.nm sc.ty)
        #:attr req #'#f))

  (define (syntax->serializable-source stx)
    (define src (syntax-source stx))
    (if (path? src)
      (path->string src)
      src))
)
;; -----------------------------------------------------------------------------

(define-syntax (require/typed/check stx)
  (syntax-parse stx
   [(_ lib clause* ...)
    (begin
      (if (or disable-require-typed-check? (not (typed-lib? #'lib)))
        ;; then : do a normal require/typed
        (quasisyntax/loc stx (#,(datum->syntax stx 'require/typed) lib clause* ...))
        ;; else : do a no-check require/typed, but do check the type annotations
        (syntax-parse #'(clause* ...)
         [((~var c* (clause #'lib)) ...)
          #:with (req* ...) (for/list ([req (in-list (syntax-e #'(c*.req ...)))]
                                       #:when (syntax-e req))
                              req)
          #:with (ann* ...) (for/list ([ann (in-list (syntax-e #'(c*.ann ...)))]
                                       #:when (syntax-e ann))
                              ann)
          (quasisyntax/loc stx
            (begin
              (require/typed/no-check lib clause* ...)
              req* ...        ;; Import opaque types
              (void ann* ...) ;; Check user-defined type annotations
              ))])))]
   ;; Default to `require/typed` on bad syntax
   [(_ lib)
    (quasisyntax/loc stx (#,(datum->syntax stx 'require/typed) lib))]
   [_:id
    (quasisyntax/loc stx #,(datum->syntax stx 'require/typed))]))
