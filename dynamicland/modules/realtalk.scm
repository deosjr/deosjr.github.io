(define-module (realtalk)
  #:use-module (scheme base)
  #:use-module (dom js)
  #:use-module (minikanren)
  #:use-module (datalog)
  #:use-module (hoot ffi)
  #:use-module (hoot hashtables)
  #:use-module (hoot gensym)
  #:export (Claim Wish When
            make-page-code
            add-page
            get-page
            get-pages
            get-dl
            recalculate-pages))

; RealTalk
; note: 'this' will have to be set within each page execution somehow?
; code to be executed is compiled in 'when' so we inject it there using (lambda (page) f ...)

; known bugs:
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

; the 'when' macro is like dl-rule, we can't use dl-rule directly because we need to have the (lambda (this) ..) part unescaped
(define-syntax When
  (lambda (stx)
    (define (symbol-with-question-mark? s)
      (and (symbol? s)
           (let ((str (symbol->string s)))
             (and (positive? (string-length str))
                  (char=? (string-ref str 0) #\?)))))

  (define (collect-vars datum)
        (cond
          [(symbol? datum)
           (if (symbol-with-question-mark? datum) (list datum) '())]
          [(pair? datum)
             (append (collect-vars (car datum))
                     (collect-vars (cdr datum)))]
          [else '()]))

  (define (remove-duplicates syms)
      (define seen '())
      (define (unique s)
        (let ((d (syntax->datum s)))
          (if (member d seen) #f
              (begin (set! seen (cons d seen)) #t))))
      (filter unique syms))

  (define (replace-symbols datum sym->gen)
    (cond
      [(symbol? datum)
       (let ((mapped (assoc datum sym->gen)))
         (if mapped (cdr mapped) (datum->syntax stx datum)))]
      [(pair? datum)
       (cons (replace-symbols (car datum) sym->gen)
             (replace-symbols (cdr datum) sym->gen))]
      [else (datum->syntax stx datum)]))

  (syntax-case stx (do)
    ((_ ((condition cx cy) ...) do statement ...)
       (with-syntax ((this (datum->syntax stx 'this)))
         (let* ((datums (syntax->datum #'((cx condition cy) ...)))
            (vars (remove-duplicates (collect-vars datums)))
            (numvars (+ 1 (length vars)))
            (gens (generate-temporaries vars))
            (sym->gen (map cons vars gens))
            (st-datums (syntax->datum #'(statement ...)))
            (replaced-statements (replace-symbols st-datums sym->gen))
            (replaced-conditions (replace-symbols datums sym->gen)))
       #`(let* ((code `,(lambda (this #,@gens) (begin #,@replaced-statements)))
                (code-name (gensym))
                (rule (fresh-vars #,numvars (lambda (q #,@gens)
                          (conj (equalo q (list this 'code (cons code-name (list #,@gens))))
                                (dl-findo (get-dl) #,replaced-conditions))))))
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
    (set-extend! (datalog-idb dl) new)
    (for-each (lambda (fact) (dl-update-indices! dl fact)) new)
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

(define (get-pages) *pages*)

(define *mouse-down* #f)
(define *mouse-offset-x* 0)
(define *mouse-offset-y* 0)

(define (div-on-press div e)
  (set-z-index! div "1")
  (set! *mouse-offset-x* (- (offset-left div) (mouse-x e)))
  (set! *mouse-offset-y* (- (offset-top div) (mouse-y e)))
  (set! *mouse-down* #t))

(define (div-on-release div e)
    (set-z-index! div "0")
    (recalculate-pages)
    (set! *mouse-down* #f))

(define (div-on-move div pid e first-e)
  (prevent-default e)
  (if *mouse-down*
    (let ((newleft (+ (mouse-x first-e) *mouse-offset-x*))
          (newtop (+ (mouse-y first-e) *mouse-offset-y*)))
      (set-z-index! div "1")
      (set-style-left! div (format #f "~apx" newleft))
      (set-style-top! div (format #f "~apx" newtop))
      (let* ((table (get-element-by-id "table"))
             (on-table ((on-table? table) pid))
             (last-known-location (hashtable-ref *page-locations* pid #f)))
        (if on-table (update-page-geometry pid div))
        (if (and on-table (not last-known-location))
          (begin
            (hashtable-set! *page-locations* pid table)
            (page-moved-onto-table table pid))
          (if (and (not on-table) last-known-location)
            (begin
              (hashtable-delete! *page-locations* pid) ; assumes single table in dom
              (page-moved-from-table table pid))))))))

(define (make-div-draggable div pid)
  (add-event-listener! div "mousedown" (procedure->external (lambda (e)
    (div-on-press div e))))
  (add-event-listener! div "mouseup" (procedure->external (lambda (e)
    (div-on-release div e))))
  (add-event-listener! div "mousemove" (procedure->external (lambda (e)
    (div-on-move div pid e e))))
; duplicate for touch events
  (add-event-listener! div "touchstart" (procedure->external (lambda (e)
    (div-on-press div (first-touch e)))))
  (add-event-listener! div "touchend" (procedure->external (lambda (e)
    (div-on-release div e))))
  (add-event-listener! div "touchmove" (procedure->external (lambda (e)
    (div-on-move div pid e (first-touch e))))))

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
    (dl-assert! dl pid '(page height) div-height)))

(define (retract-page-geometry pid)
  (let (( left (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid (page left) ,x) ))))))
        ( top (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid (page top) ,x) ))))))
        ( width (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid (page width) ,x) ))))))
        ( height (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid (page height) ,x) )))))))
    (if (not (null? left)) (dl-retract! dl `(,pid (page left) ,(car left))))
    (if (not (null? top)) (dl-retract! dl `(,pid (page top) ,(car top))))
    (if (not (null? width)) (dl-retract! dl `(,pid (page width) ,(car width))))
    (if (not (null? height)) (dl-retract! dl `(,pid (page height) ,(car height))))))

; assumption: style.transform format is 'rotate(<DEGREES>deg)'
(define (get-div-rotation div)
  (let* ((str (get-transform div))
         (deg-str (substring str 7 (- (string-length str) 4))))
    (string->number deg-str)))

(define (update-page-rotation div pid n)
  (let* ((div-rotation (get-div-rotation div))
         (table (get-element-by-id "table"))
         (on-table ((on-table? table) pid)))
    (set-style-transform! div (format #f "rotate(~adeg)" (modulo (+ (get-div-rotation div) n) 360) ))
    (if on-table
      (let ((rotation (dl-find (fresh-vars 1 (lambda (x) (dl-findo dl ( (,pid (page rotation) ,x) )))))))
        (if (not (null? rotation)) (dl-retract! dl `(,pid (page rotation) ,(car rotation))))
        (dl-assert! dl pid '(page rotation) div-rotation)
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
  (set-inner-html! (get-element-by-id "table") "")
  (for-each (lambda (pid) (reset-page-style! (hashtable-ref *divs* pid #f))) *pages*)
  ; todo: do we need to reset dl-idb ?
  ; currently rules execute each time a page is moved, which is not what I'd expect
  (dl-fixpoint! dl))

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
