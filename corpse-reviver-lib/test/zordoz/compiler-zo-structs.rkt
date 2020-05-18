#lang racket/base
(define-struct zo () #:prefab)
(provide (struct-out zo))
(begin
 (define-struct (function-shape zo) ((arity) (preserves-marks?)) #:prefab)
 (provide (struct-out function-shape)))
(begin
 (define-struct (struct-shape zo) () #:prefab)
 (provide (struct-out struct-shape)))
(begin
 (define-struct (constructor-shape struct-shape) ((arity)) #:prefab)
 (provide (struct-out constructor-shape)))
(begin
 (define-struct (predicate-shape struct-shape) () #:prefab)
 (provide (struct-out predicate-shape)))
(begin
 (define-struct (accessor-shape struct-shape) ((field-count)) #:prefab)
 (provide (struct-out accessor-shape)))
(begin
 (define-struct (mutator-shape struct-shape) ((field-count)) #:prefab)
 (provide (struct-out mutator-shape)))
(begin
 (define-struct (struct-type-shape struct-shape) ((field-count)) #:prefab)
 (provide (struct-out struct-type-shape)))
(begin
 (define-struct
   (struct-type-property-shape struct-shape)
   ((has-guard?))
   #:prefab)
 (provide (struct-out struct-type-property-shape)))
(begin
 (define-struct (property-predicate-shape struct-shape) () #:prefab)
 (provide (struct-out property-predicate-shape)))
(begin
 (define-struct (property-accessor-shape struct-shape) () #:prefab)
 (provide (struct-out property-accessor-shape)))
(begin
 (define-struct (struct-other-shape struct-shape) () #:prefab)
 (provide (struct-out struct-other-shape)))
(begin
 (define-struct (global-bucket zo) ((name)) #:prefab)
 (provide (struct-out global-bucket)))
(begin
 (define-struct
   (module-variable zo)
   ((modidx) (sym) (pos) (phase) (constantness))
   #:prefab)
 (provide (struct-out module-variable)))
(begin
 (define-struct
   (prefix zo)
   ((num-lifts) (toplevels) (stxs) (src-inspector-desc))
   #:prefab)
 (provide (struct-out prefix)))
(begin (define-struct (form zo) () #:prefab) (provide (struct-out form)))
(begin (define-struct (expr form) () #:prefab) (provide (struct-out expr)))
(begin
 (define-struct
   (compilation-top zo)
   ((max-let-depth) (binding-namess) (prefix) (code))
   #:prefab)
 (provide (struct-out compilation-top)))
(begin
 (define-struct
   (provided zo)
   ((name) (src) (src-name) (nom-src) (src-phase) (protected?))
   #:prefab)
 (provide (struct-out provided)))
(begin
 (define-struct
   (toplevel expr)
   ((depth) (pos) (const?) (ready?))
   #:prefab)
 (provide (struct-out toplevel)))
(begin
 (define-struct (seq form) ((forms)) #:prefab)
 (provide (struct-out seq)))
(begin
 (define-struct
   (seq-for-syntax form)
   ((forms) (prefix) (max-let-depth) (dummy))
   #:prefab)
 (provide (struct-out seq-for-syntax)))
(begin
 (define-struct (inline-variant form) ((direct) (inline)) #:prefab)
 (provide (struct-out inline-variant)))
(begin
 (define-struct (def-values form) ((ids) (rhs)) #:prefab)
 (provide (struct-out def-values)))
(begin
 (define-struct
   (def-syntaxes form)
   ((ids) (rhs) (prefix) (max-let-depth) (dummy))
   #:prefab)
 (provide (struct-out def-syntaxes)))
(begin
 (define-struct
   (mod form)
   ((name)
    (srcname)
    (self-modidx)
    (prefix)
    (provides)
    (requires)
    (body)
    (syntax-bodies)
    (unexported)
    (max-let-depth)
    (dummy)
    (lang-info)
    (internal-context)
    (binding-names)
    (flags)
    (pre-submodules)
    (post-submodules))
   #:prefab)
 (provide (struct-out mod)))
(begin
 (define-struct
   (lam expr)
   ((name)
    (flags)
    (num-params)
    (param-types)
    (rest?)
    (closure-map)
    (closure-types)
    (toplevel-map)
    (max-let-depth)
    (body))
   #:prefab)
 (provide (struct-out lam)))
(begin
 (define-struct (closure expr) ((code) (gen-id)) #:prefab)
 (provide (struct-out closure)))
(begin
 (define-struct (case-lam expr) ((name) (clauses)) #:prefab)
 (provide (struct-out case-lam)))
(begin
 (define-struct (let-one expr) ((rhs) (body) (type) (unused?)) #:prefab)
 (provide (struct-out let-one)))
(begin
 (define-struct (let-void expr) ((count) (boxes?) (body)) #:prefab)
 (provide (struct-out let-void)))
(begin
 (define-struct
   (install-value expr)
   ((count) (pos) (boxes?) (rhs) (body))
   #:prefab)
 (provide (struct-out install-value)))
(begin
 (define-struct (let-rec expr) ((procs) (body)) #:prefab)
 (provide (struct-out let-rec)))
(begin
 (define-struct (boxenv expr) ((pos) (body)) #:prefab)
 (provide (struct-out boxenv)))
(begin
 (define-struct
   (localref expr)
   ((unbox?) (pos) (clear?) (other-clears?) (type))
   #:prefab)
 (provide (struct-out localref)))
(begin
 (define-struct (topsyntax expr) ((depth) (pos) (midpt)) #:prefab)
 (provide (struct-out topsyntax)))
(begin
 (define-struct (application expr) ((rator) (rands)) #:prefab)
 (provide (struct-out application)))
(begin
 (define-struct (branch expr) ((test) (then) (else)) #:prefab)
 (provide (struct-out branch)))
(begin
 (define-struct (with-cont-mark expr) ((key) (val) (body)) #:prefab)
 (provide (struct-out with-cont-mark)))
(begin
 (define-struct (beg0 expr) ((seq)) #:prefab)
 (provide (struct-out beg0)))
(begin
 (define-struct (splice form) ((forms)) #:prefab)
 (provide (struct-out splice)))
(begin
 (define-struct (varref expr) ((toplevel) (dummy)) #:prefab)
 (provide (struct-out varref)))
(begin
 (define-struct (assign expr) ((id) (rhs) (undef-ok?)) #:prefab)
 (provide (struct-out assign)))
(begin
 (define-struct (apply-values expr) ((proc) (args-expr)) #:prefab)
 (provide (struct-out apply-values)))
(begin
 (define-struct (with-immed-mark expr) ((key) (def-val) (body)) #:prefab)
 (provide (struct-out with-immed-mark)))
(begin
 (define-struct (primval expr) ((id)) #:prefab)
 (provide (struct-out primval)))
(begin
 (define-struct (req form) ((reqs) (dummy)) #:prefab)
 (provide (struct-out req)))
(begin
 (define-struct (stx zo) ((content)) #:prefab)
 (provide (struct-out stx)))
(begin
 (define-struct
   (stx-obj zo)
   ((datum) (wrap) (srcloc) (props) (tamper-status))
   #:prefab)
 (provide (struct-out stx-obj)))
(begin
 (define-struct
   (wrap zo)
   ((shifts) (simple-scopes) (multi-scopes))
   #:prefab)
 (provide (struct-out wrap)))
(begin
 (define-struct
   (module-shift zo)
   ((from) (to) (from-inspector-desc) (to-inspector-desc))
   #:prefab)
 (provide (struct-out module-shift)))
(begin
 (define-struct
   (scope zo)
   ((name)
    (kind)
    (bindings #:mutable)
    (bulk-bindings #:mutable)
    (multi-owner #:mutable))
   #:prefab)
 (provide (struct-out scope)))
(begin
 (define-struct
   (multi-scope zo)
   ((name) (src-name) (scopes #:mutable))
   #:prefab)
 (provide (struct-out multi-scope)))
(begin
 (define-struct (binding zo) () #:prefab)
 (provide (struct-out binding)))
(begin
 (define-struct
   (free-id=?-binding binding)
   ((base) (id) (phase))
   #:prefab)
 (provide (struct-out free-id=?-binding)))
(begin
 (define-struct (local-binding binding) ((name)) #:prefab)
 (provide (struct-out local-binding)))
(begin
 (define-struct (module-binding binding) ((encoded)) #:prefab)
 (provide (struct-out module-binding)))
(begin
 (define-struct
   (decoded-module-binding binding)
   ((path)
    (name)
    (phase)
    (nominal-path)
    (nominal-export-name)
    (nominal-phase)
    (import-phase)
    (inspector-desc))
   #:prefab)
 (provide (struct-out decoded-module-binding)))
(begin
 (define-struct
   (all-from-module zo)
   ((path) (phase) (src-phase) (inspector-desc) (exceptions) (prefix))
   #:prefab)
 (provide (struct-out all-from-module)))
