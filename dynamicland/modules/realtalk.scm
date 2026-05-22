(define-module (realtalk)
  #:use-module (scheme base)
  #:use-module (scheme eval)
  #:use-module (scheme write)
  #:use-module (dom js)
  #:use-module (minikanren)
  #:use-module (datalog)
  #:use-module (hoot ffi)
  #:use-module (hoot gensym)
  #:use-module (hoot hashtables)
  #:use-module (hoot modules)
  #:use-module (hoot ports)
  #:use-module (hoot read)
  #:export (Claim Wish When
            derived-claim! derived-wish!
            make-page-code
            make-dynamic
            add-page
            get-page
            get-pages
            add-keyboard
            get-dl
            recalculate-pages
            eval-string
            recode-page!))

; RealTalk
; note: 'this' will have to be set within each page execution somehow?
; code to be executed is compiled in 'when' so we inject it there using (lambda (page) f ...)

; known bugs:
; - not fully matching/unpacking a complex list in When condition leads to arbitrary multiple executions?
; -> structurally identical lists can still hash differently...
; -> due to recursion budget in hash function
; - derived Claim/Wish is not supported in When macro, 'this' keyword not available
; --> see below, inject 'this' explicitly in embedded Claim/Wish. Disallow nested When rules
; - derived Claim/Wish is not supported in When macro, behaviour should be different
; --> When macro replaces Claim/Wish with DerivedClaim/DerivedWish?

(define *rule-procs* (make-hashtable))

(define-syntax Claim
  (lambda (stx)
    (syntax-case stx ()
      ((_ id attr value)
       (with-syntax ((this (datum->syntax stx 'this)))
         #'(begin
             (dl-assert! (get-dl) this 'claims (list id attr value))
             (dl-assert! (get-dl) id attr value)))))))

(define-syntax Wish
  (lambda (stx)
    (syntax-case stx ()
      ((_ id attr value)
       (with-syntax ((this (datum->syntax stx 'this)))
       #'(dl-assert! (get-dl) this 'wishes (list id attr value)))))))

; Used by the When macro to implement Claim/Wish *inside* a rule body.
; Mirror of Claim/Wish but routed through dl-assert-derived! so writes land in
; the IDB (re-derived next iteration if the rule still fires, retracted by the
; next dl-fixpoint! reset when it stops).
(define (derived-claim! this id attr value)
  (dl-assert-derived! (get-dl) this 'claims (list id attr value))
  (dl-assert-derived! (get-dl) id attr value))

(define (derived-wish! this id attr value)
  (dl-assert-derived! (get-dl) this 'wishes (list id attr value)))

#|
(define-syntax When
  (lambda (stx)
    (syntax-case stx (wishes do)
    ((_ (condition ...) do statement ... )
       (with-syntax ((this (datum->syntax stx 'this)))
           #'(dl-rule (code this (lambda (this) (begin statement ...))) :- condition ...)))
    ((_ someone wishes w do statement ... )
       (with-syntax ((this (datum->syntax stx 'this)))
           #'(dl-rule (code this (lambda (this) (begin statement ...))) :- (wishes someone w) ))))))
|#

; ----------------------------------------------------------------------
; (When (cond ...) do body ...)
;
; A rule with side effects. We compile each use site to two cooperating
; pieces wired together by a unique symbol:
;
;   1. A `rule` value: a minikanren goal that unifies the result `q` with a
;      tuple `(this 'code (code-name . args))`. When dl-fixpoint-iterate
;      runs the rules, satisfying the conditions produces one such tuple
;      per match, with `args` bound to the logic-var values.
;
;   2. A `code` procedure: the rule's body wrapped as
;      (lambda (this ?var1 ?var2 ...) body...). Stored in *rule-procs*
;      under code-name. After dl-fixpoint-iterate accumulates new facts it
;      walks them, looks each proc up by code-name, and applies it to the
;      bound args. That's where the body's side effects run.
;
; So the When macro never executes the body directly — it just packages
; the body for later application during fixpoint iteration.
;
; A few subtleties:
;
; * Logic variables (`?x`, `?p`, ...) are not legal Scheme identifiers, so
;   we can't use syntax-case patterns to bind them. We work on the datum
;   tree (via syntax->datum), substitute each ?var with a fresh gensym,
;   then datum->syntax everything back. The gensym replacement gives us
;   per-rule hygiene: two When forms that both use ?x get distinct
;   identifiers, no accidental capture.
;
; * Re-anchoring free identifiers: when we datum->syntax a symbol like a
;   user-defined helper procedure, we pass `stx` (the macro input) as the
;   context so the symbol resolves at the *user's* source location, not
;   in this realtalk module. Without that, calls to user helpers from
;   inside the body wouldn't resolve.
;
; * `this` is anchored once via with-syntax so the rule-lambda's `this`
;   parameter, the body's `this` references, and the (Claim/Wish ...) ->
;   (derived-claim!/wish! this ...) rewrites all refer to the same
;   identifier — and so the lambda binding captures every body reference.
;
; The xform pass below does all of this in one walk:
;   ?var          -> its assigned gensym
;   (Claim ...)   -> (derived-claim! this ...)   ; live for one fixpoint
;   (Wish  ...)   -> (derived-wish!  this ...)   ; same
;   (When  ...)   -> compile-time error (lifecycle of nested rules unclear)
;   other symbol  -> re-anchored to user's stx
;   pair          -> recurse on car and cdr
; ----------------------------------------------------------------------
(define-syntax When
  (lambda (stx)
    ; A logic variable: any symbol whose first char is #\?, e.g. ?p, ?color.
    (define (logic-var? x)
      (and (symbol? x)
           (let ((s (symbol->string x)))
             (and (positive? (string-length s))
                  (char=? (string-ref s 0) #\?)))))

    ; All distinct logic variables in DATUM, in first-seen order. The order
    ; matters: it fixes the parameter order of both the body-procedure and
    ; the rule's lambda, so the args produced by minikanren match up.
    (define (collect-logic-vars datum)
      (let walk ((d datum) (seen '()))
        (cond ((logic-var? d) (if (member d seen) seen (cons d seen)))
              ((pair? d) (walk (cdr d) (walk (car d) seen)))
              (else seen))))

    ; Transform DATUM for splicing back into the emitted code:
    ;   - SYM->GEN maps each ?var to its chosen gensym (a syntax object).
    ;   - All other symbols become syntax objects anchored to stx (the
    ;     macro input), so they resolve at the user's source location.
    (define (xform datum sym->gen)
      (define (recur d) (xform d sym->gen))
      (define (here sym) (datum->syntax stx sym))
      (cond
        ; ?var: substitute its gensym if we know one; otherwise it must be
        ; a free ?var (none introduced in the conditions). Anchor it as a
        ; bare symbol; if it survives to runtime it'll be an unbound error.
        ((logic-var? datum)
         (cond ((assq datum sym->gen) => cdr)
               (else (here datum))))
        ; Atom (non-pair): just re-anchor.
        ((not (pair? datum)) (here datum))
        ; (Claim id attr v) inside this body becomes a *derived* claim that
        ; lives in the IDB for one fixpoint iteration. We inject `this`
        ; explicitly so the form doesn't depend on the Claim macro's own
        ; hygiene to find it.
        ((eq? (car datum) 'Claim)
         (cons (here 'derived-claim!) (cons (here 'this) (map recur (cdr datum)))))
        ; (Wish id attr v) — same treatment as Claim.
        ((eq? (car datum) 'Wish)
         (cons (here 'derived-wish!) (cons (here 'this) (map recur (cdr datum)))))
        ; (When ...) nested inside a When body is rejected at expand time:
        ; the lifecycle of "a rule that produces rules" is not designed.
        ((eq? (car datum) 'When)
         (syntax-violation 'When "nested When is not supported" stx))
        ; Ordinary form: recurse on car and cdr.
        (else (cons (recur (car datum)) (recur (cdr datum))))))

    (syntax-case stx (do)
      ((_ ((condition cx cy) ...) do statement ...)
       ; Bind `this` once as a pattern variable anchored to the user's stx,
       ; so every reference to `this` below (template lambda parameter,
       ; (dl-assert! ... this ...) call, and xform-injected `this`s) all
       ; expand to the same identifier.
       (with-syntax ((this (datum->syntax stx 'this)))
         (let* ((conds (syntax->datum #'((cx condition cy) ...)))
                (body  (syntax->datum #'(statement ...)))
                ; Walk conds *and* body together so a ?var used only in
                ; the body (e.g. shadowing a condition var) still gets a
                ; gensym slot; the rule-lambda passes it through unbound,
                ; minikanren handles it.
                (vars  (collect-logic-vars (cons conds body)))
                (gens  (generate-temporaries vars))
                (sym->gen (map cons vars gens))
                (conds* (xform conds sym->gen))
                (body*  (xform body  sym->gen)))
           ; Emitted shape:
           ;   (let* ((code      (lambda (this g1 g2 ...) body*))
           ;          (code-name (gensym))
           ;          (rule      (fresh-vars N
           ;                       (lambda (q g1 g2 ...)
           ;                         (conj
           ;                           ; unify q with the result tuple the
           ;                           ; engine looks up code-name in
           ;                           (equalo q (list this 'code
           ;                                           (cons code-name
           ;                                                 (list g1 g2 ...))))
           ;                           ; the user-written conditions
           ;                           (dl-findo (get-dl) conds*))))))
           ;     (hashtable-set! *rule-procs* code-name code)  ; register
           ;     (dl-assert! (get-dl) this 'rules rule)        ; for retraction
           ;     (dl-assert-rule! (get-dl) rule))              ; make it active
           ;
           ; N is (1 + length vars): the rule procedure takes `q` (its
           ; output unification slot) plus one gensym per logic var.
           #`(let* ((code (lambda (this #,@gens) #,@body*))
                    (code-name (gensym))
                    (rule (fresh-vars #,(+ 1 (length vars))
                            (lambda (q #,@gens)
                              (conj (equalo q (list this 'code
                                                    (cons code-name (list #,@gens))))
                                    (dl-findo (get-dl) #,conds*))))))
               (hashtable-set! *rule-procs* code-name code)
               (dl-assert! (get-dl) this 'rules rule)
               (dl-assert-rule! (get-dl) rule))))))))

(define-syntax make-page-code
  (lambda (stx)
    (syntax-case stx ()
      ((_ body ...)
       (with-syntax ((this (datum->syntax stx 'this)))
         #'(lambda (this) body ...))))))

; TEMPORARY: we want to associate a table with a datalog instance, and inject the relevant one
; for now we hardcode a single instance
; todo: make-dynamic takes arguments for background pages
(define (make-dynamic)
    (let* ((table-div (get-element-by-id "table"))
           (table-rect (get-bounding-client-rect table-div))
           (tw (get-width table-rect))
           (th (get-height table-rect))
           (svg (make-svg-element "svg"))
           (other (make-element "other")))
      (add-event-listener! (window) "keydown" (procedure->external (lambda (e)
        (let* ((keystr (get-key e))
               (key (string-ref keystr 0)))
          ; ignore modifier keys for now
          (if (= (string-length keystr) 1)
            (begin
              (set! last-key key)
              (set! just-pressed #t)
      ))))))
      (set-attribute! svg "xmlns" "http://www.w3.org/2000/svg")
      (set-attribute! svg "width" (format #f "~a" tw))
      (set-attribute! svg "height" (format #f "~a" th))
      (append-child! table-div svg)
      (append-child! table-div other)))

(define dl (make-new-datalog))
(define (get-dl) dl)

; redefine dl-fixpoint! injecting code execution as result of rules
(define (dl-fixpoint! dl)
  (for-each (lambda (fact) (dl-retract! dl fact)) (hashtable-keys (datalog-idb dl)))
  (set-datalog-idb! dl (make-hashtable))
  (dl-fixpoint-iterate dl))

(define (dl-fixpoint-iterate dl)
  (let* ((facts (map (lambda (rule) (dl-apply-rule dl rule)) (hashtable-keys (datalog-rdb dl))))
         (factset (foldl (lambda (x y) (set-extend! y x)) facts (make-hashtable)))
         (new (hashtable-keys (set-difference factset (datalog-idb dl)))))
    (for-each (lambda (fact)
                (dl-assert-derived! dl (car fact) (cadr fact) (caddr fact))) new)
    ; result of dl_apply_rule should be a tuple (this 'code (proc . args))
    (for-each (lambda (c)
      (let ((this (car c))
            (proc (caaddr c))
            (args (cdaddr c)))
         (apply (hashtable-ref *rule-procs* proc #f) this args))) new)
    (if (not (null? new)) (dl-fixpoint-iterate dl))))

(define *pages* '())
(define *procs* (make-hashtable))
(define *divs* (make-hashtable))
(define *page-locations* (make-hashtable))

(define (get-page pid)
  (hashtable-ref *divs* pid #f))

(define (add-page proc)
  (let* ((pid (dl-record! dl 'page ('code proc)))
         (div (make-page-div pid)))
    (set! *pages* (cons pid *pages*))
    (hashtable-set! *procs* pid proc)
    (hashtable-set! *divs* pid div)
    pid))

(define (make-page-div id)
  (let ((div (make-element "div")))
    (hashtable-set! *divs* id div)
    (set-attribute! div "class" "page")
    (set-attribute! div "tabindex" "0")
    (set-attribute! div "id" (number->string id))
    (set-style-transform! div "rotate(0deg)")
    (make-div-draggable div id)
    (make-div-focusable div id) div))

; only support a single keyboard for now, since we are in the browser anyways
(define last-key #f)
(define just-pressed #f)

(define (keyboard-proc this)
  (Claim this 'keyboard #t)
  (Wish this 'has-whiskers #t)

  (define (claim-key-pressed)
    (if just-pressed (begin
      (set! just-pressed #f)
      (hashtable-set! (datalog-idb dl) `(,this claims (,this key-pressed ,last-key)) #t)
      (hashtable-set! (datalog-idb dl) `(,this key-pressed ,last-key) #t)
      (Claim this 'key-pressed last-key)
  )))

  (When ((keyboard ,this #t)) do
    (claim-key-pressed))
)

(define (add-keyboard)
  (let* ((pid (dl-record! dl 'page ('code keyboard-proc)))
         (div (make-page-div pid)))
    (set! *pages* (cons pid *pages*))
    (hashtable-set! *procs* pid keyboard-proc)
    (hashtable-set! *divs* pid div)
    pid))

(define (get-pages) *pages*)

; --- Dragging --------------------------------------------------------
;
; Uses Pointer Events so the same handlers cover mouse, touch, and pen.
; On press, we attach `pointermove` / `pointerup` / `pointercancel` to
; the window (not the div) and capture the pointer to this element.
; That gives us three properties:
;
;   - the drag continues even if the cursor leaves the div
;   - the release fires even if the cursor is off-card or off-window
;   - cards that aren't being dragged have zero idle listeners
;
; All drag state (offset, the handler refs we need to remove) lives in
; the closure built on press — no module-level globals.

(define (make-div-draggable div pid)
  (add-event-listener! div "pointerdown"
    (procedure->external (lambda (e) (begin-drag div pid e)))))

(define (begin-drag div pid e)
  (prevent-default e)
  (set-pointer-capture! div (pointer-id e))
  (add-class! div "dragging")
  ; Anchor: the offset from the cursor to the div's origin at press time.
  ; Use offsetLeft/offsetTop (not getBoundingClientRect) because those are
  ; in the same coordinate system as CSS `left`/`top` — i.e. relative to
  ; the offset parent. getBoundingClientRect would give viewport coords,
  ; which differ from style.left/top by the page scroll offset and cause
  ; dragged divs to jump to scrollY=0 when the page isn't at the top.
  (let* ((dx (- (offset-left div) (mouse-x e)))
         (dy (- (offset-top  div) (mouse-y e)))
         ; on-move and on-end are mutually recursive references through
         ; the closure: on-end needs to pass on-move and itself to
         ; remove-event-listener!. Set! after define gives us that.
         (on-move #f)
         (on-end  #f))
    (set! on-move (procedure->external
                    (lambda (e) (drag-to div pid e dx dy))))
    (set! on-end  (procedure->external
                    (lambda (e) (end-drag div on-move on-end))))
    (add-event-listener! (window) "pointermove"   on-move)
    (add-event-listener! (window) "pointerup"     on-end)
    (add-event-listener! (window) "pointercancel" on-end)))

(define (drag-to div pid e dx dy)
  (let ((x (+ (mouse-x e) dx))
        (y (+ (mouse-y e) dy)))
    ; We use left/top rather than transform: translate because `transform`
    ; is owned by the rotation system (update-page-rotation). Mixing the
    ; two would require composing them via CSS custom properties — worth
    ; doing if drag perf becomes an issue.
    (set-style-left! div (format #f "~apx" x))
    (set-style-top!  div (format #f "~apx" y))
    (let* ((table (get-element-by-id "table"))
           (on-table ((on-table? table) pid))
           (last (hashtable-ref *page-locations* pid #f)))
      (if on-table (update-page-geometry pid div))
      (cond ((and on-table (not last))
             (hashtable-set! *page-locations* pid table)
             (page-moved-onto-table table pid))
            ((and (not on-table) last)
             (hashtable-delete! *page-locations* pid)
             (page-moved-from-table table pid))))))

(define (end-drag div on-move on-end)
  (remove-event-listener! (window) "pointermove"   on-move)
  (remove-event-listener! (window) "pointerup"     on-end)
  (remove-event-listener! (window) "pointercancel" on-end)
  (remove-class! div "dragging")
  (recalculate-pages))

(define (make-div-focusable div pid)
  (add-event-listener! div "mouseenter" (procedure->external (lambda (e)
    (focus div))))
  (add-event-listener! div "keydown" (procedure->external (lambda (e)
    (let ((key (string-ref (get-key e) 0))
          (rot 15))
      (if (eq? key #\q) (update-page-rotation div pid (- rot))
      (if (eq? key #\e) (update-page-rotation div pid rot))))))))

; make page div dimensions known in datalog
; page dimensions are relative to the table they are on!
(define (update-page-geometry pid div)
  (let* ((div (hashtable-ref *divs* pid #f))
         (div-rotation (get-div-rotation div))
         (div-rect (get-bounding-client-rect div))
         (table (get-element-by-id "table"))
         (table-rect (get-bounding-client-rect table))
         (divx (get-x div-rect))
         (divy (get-y div-rect))
         (tablex (get-x table-rect))
         (tabley (get-y table-rect))
         (div-width (get-width div-rect))
         (div-height (get-height div-rect)))
    (retract-page-geometry pid)
    (dl-assert! dl pid '(page left) (- divx tablex))
    (dl-assert! dl pid '(page top) (- divy tabley))
    (dl-assert! dl pid '(page width) div-width)
    (dl-assert! dl pid '(page height) div-height)
    (dl-assert! dl pid '(page rotation) div-rotation)))

(define (retract-page-geometry pid)
  (let (( left (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid (page left) ,x) ))))))
        ( top (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid (page top) ,x) ))))))
        ( width (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid (page width) ,x) ))))))
        ( height (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid (page height) ,x) ))))))
        ( rotation (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid (page rotation) ,x) )))))))
    (if (not (null? left)) (dl-retract! dl `(,pid (page left) ,(car left))))
    (if (not (null? top)) (dl-retract! dl `(,pid (page top) ,(car top))))
    (if (not (null? width)) (dl-retract! dl `(,pid (page width) ,(car width))))
    (if (not (null? height)) (dl-retract! dl `(,pid (page height) ,(car height))))
    (if (not (null? rotation)) (dl-retract! dl `(,pid (page rotation) ,(car rotation))))))

; assumption: style.transform format is 'rotate(<DEGREES>deg)'
(define (get-div-rotation div)
  (let* ((str (get-transform div))
         (deg-str (substring str 7 (- (string-length str) 4))))
    (string->number deg-str)))

(define (update-page-rotation div pid n)
  (let* ((div-rotation (get-div-rotation div))
         (new-rotation (modulo (+ (get-div-rotation div) n) 360))
         (table (get-element-by-id "table"))
         (on-table ((on-table? table) pid)))
    (set-style-transform! div (format #f "rotate(~adeg)" new-rotation ))
    (if on-table
      (let ((rotation (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid (page rotation) ,x) )))))))
        (if (not (null? rotation)) (dl-retract! dl `(,pid (page rotation) ,(car rotation))))
        (dl-assert! dl pid '(page rotation) new-rotation)
        (recalculate-pages)))))

; only run page code when newly in bounds of table
(define (page-moved-onto-table table pid)
  (execute-page pid)
  (recalculate-pages))

; then retract all 'this claims x' and 'this rules x' from dl-db when newly out of table bounds
(define (page-moved-from-table table pid)
  (let (( claims (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid claims ,x) ))))))
        ( wishes (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid wishes ,x) ))))))
        ( rules  (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid rules ,x) )))))))
    (for-each (lambda (claim) (dl-retract! dl claim)) claims)
    (for-each (lambda (claim) (dl-retract! dl `(,pid claims ,claim))) claims)
    (for-each (lambda (wish) (dl-retract! dl `(,pid wishes ,wish))) wishes)
    (for-each (lambda (rule) (dl-retract-rule! dl rule)) rules)
    (for-each (lambda (rule) (dl-retract! dl `(,pid rules ,rule))) rules))
  (recalculate-pages))

; NOTE: idea, discarded (reversible effects):
; effects will need to be explicitly undone. This needs reversible effects, and the reverse-function can perhaps also use the dl-db?
; instead of resetting all pages each loop, we can run all reverse-effects caused by a paper that just left the table
; reverse of set-background! would be set-background! to "", for example. fixpoint should run after to mitigate clashes, theyre undefined anyways
(define (reset-page-style! pagediv)
  (let ((left (get-left pagediv))
        (top (get-top pagediv))
        (transform (get-transform pagediv))
        (z (get-z-index pagediv)))
    (set-attribute! pagediv "class" "page")
    (set-style! pagediv "")
    (set-style-left! pagediv left)
    (set-style-top! pagediv top)
    (set-style-transform! pagediv transform)
    (set-z-index! pagediv z)))

; When a page is in view, its code is executed. Then when all pages have ran, dl-fixpoint runs all consequences.
; assumes a single table for now, a div with id "table"
; TODO: keep a mapping of tables->pages, and only run a page when it is newly detected on a table
; when a page is removed from the table, retract all when-rules it introduced and all claims/wishes it asserted into that tables' datalog instance.
; then remove all derived facts and run fixpoint analysis again. This way we can encapsulate state in page code!
; NOTE: there are no derived facts!!! only followup claims/rules. we can query datalog to get all claims/rules asserted by a page as we run a closure over 'this' when creating rule lambda
(define (recalculate-pages)
  (let ((table-div (get-element-by-id "table")))
    ; table has two child divs: <svg> and <other>
    ; the latter can contain things like images, but svg is preferred for most projections
    (set-inner-html! (query-selector table-div "svg") "")
    (set-inner-html! (query-selector table-div "other") "")
    (for-each (lambda (pid) (reset-page-style! (hashtable-ref *divs* pid #f))) *pages*)
    (assert-time)
    ; todo: do we need to reset dl-idb ?
    ; currently rules execute each time a page is moved, which is not what I'd expect
    (dl-fixpoint! dl)))

(define (assert-time)
  (let (( claims (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (now time ,x) )))))))
    (for-each (lambda (claim) (dl-retract! dl `(now time ,claim))) claims)
    (dl-assert! dl 'now 'time (date-now))))

(define (execute-page pid)
  ((hashtable-ref *procs* pid #f) pid))

(define (on-table? table)
  (lambda (pid)
    (let* ((div (hashtable-ref *divs* pid #f))
           (div-rect (get-bounding-client-rect div))
           (divx (get-x div-rect))
           (divy (get-y div-rect))
           (div-width (get-width div-rect))
           (div-height (get-height div-rect))
           (table-rect (get-bounding-client-rect table))
           (tablex (get-x table-rect))
           (tabley (get-y table-rect))
           (table-width (get-width table-rect))
           (table-height (get-height table-rect)))
      (and (> divx tablex)
           (< (+ divx div-width) (+ tablex table-width))
           (> divy tabley)
           (< (+ divy div-height) (+ tabley table-height))))))

; ---------------------------------------------------------------------
; REPL helper: read all top-level expressions from STR and eval each in
; this module. Defined here so that (current-module) at the eval call
; resolves to (realtalk), giving eval'd code access to Claim/Wish/When
; plus everything realtalk imports — (dom js), (datalog), etc.
;
; call-with-values is load-bearing: FFI procedures declared `-> none`
; return zero values, and binding a zero-value result in a single-value
; position is an arity violation in Hoot. We collapse 0 / 1 / many
; values down to one display-able value.

(define (value->display val)
  ; (if #f #f) is the unspecified value; print "ok" for it so the REPL
  ; output reads like a real REPL rather than "#!unspecified".
  (if (eq? val (if #f #f))
      "ok"
      (let ((p (open-output-string)))
        (write val p)
        (get-output-string p))))

(define (exn->display exn)
  (let ((p (open-output-string)))
    (display "error: " p)
    (display exn p)
    (get-output-string p)))

(define (eval-string str)
  (guard (exn (#t (exn->display exn)))
    (let ((port (open-input-string str)))
      (let loop ((expr (read port)) (val (if #f #f)))
        (if (eof-object? expr)
            (value->display val)
            (loop (read port)
                  (call-with-values
                      (lambda () (eval expr (current-module)))
                    (lambda vals
                      (if (null? vals) (if #f #f) (car vals))))))))))

; Live-coding helper: replace the procedure on PID with SRC compiled as a
; page body (i.e. (lambda (this) <src>)), retract any state the old
; procedure had asserted, and re-execute. Used by the live REPL page so
; that editing a textarea swaps the active code for a draggable card.
;
; The src string is wrapped with (make-page-code ...) (rather than a hand-
; rolled (lambda (this) ...)) to reuse make-page-code's `this` injection,
; which is hygienically the same as the rest of the realtalk macros.
(define (recode-page! pid src)
  (let* ((wrapped (string-append "(make-page-code " src ")"))
         (port (open-input-string wrapped))
         (proc (eval (read port) (current-module))))
    ; Retract any claims/wishes/rules the old proc had asserted. The
    ; existing page-moved-from-table helper does this; we pass #f for the
    ; table argument since its implementation ignores that field.
    (page-moved-from-table #f pid)
    (hashtable-set! *procs* pid proc)
    ; Only execute if the page is currently on the table. Otherwise the
    ; new proc just sits ready; it'll run on the next drag-onto-table.
    (when (hashtable-ref *page-locations* pid #f)
      (page-moved-onto-table #f pid))))
