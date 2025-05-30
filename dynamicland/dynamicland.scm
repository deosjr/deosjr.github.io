(use-modules (scheme base)
             (dom js)
             (minikanren)
             (datalog)
             (hoot gensym)
             (hoot hashtables)
             (hoot ffi))

; RealTalk
; note: 'this' will have to be set within each page execution somehow?
; code to be executed is compiled in 'when' so we inject it there using (lambda (page) f ...)
(define-syntax claim
  (lambda (stx)
    (syntax-case stx ()
      ((_ id attr value)
       (with-syntax ((this (datum->syntax stx 'this)))
         #'(begin
             (dl_assert this 'claims (list id attr value))
             (dl_assert id attr value)))))))

(define-syntax wish
  (lambda (stx)
    (syntax-case stx ()
      ((_ x)
       (with-syntax ((this (datum->syntax stx 'this)))
       #'(dl_assert this 'wishes 'x))))))

#|
(define-syntax when
  (lambda (stx)
    (syntax-case stx (wishes do)
    ((_ (condition ...) do statement ... )
       (with-syntax ((this (datum->syntax stx 'this)))
           #'(dl_rule (code this (lambda (this) (begin statement ...))) :- condition ...)))
    ((_ someone wishes w do statement ... )
       (with-syntax ((this (datum->syntax stx 'this)))
           #'(dl_rule (code this (lambda (this) (begin statement ...))) :- (wishes someone w) ))))))
|#

; the 'when' macro is like dl_rule, we can't use dl_rule directly because we need to have the (lambda (this) ..) part unescaped
(define-syntax when
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
            (st-replaced (replace-symbols st-datums sym->gen))
            (replaced (replace-symbols datums sym->gen)))
       #`(dl_assert_rule 
           (let ((code `,(lambda (this #,@gens) (begin #,@st-replaced))))
             (fresh-vars #,numvars (lambda (q #,@gens) (conj (equalo q (list this 'code (cons code (list #,@gens)))) (dl_findo #,replaced) )))))))))))

; redefine dl_fixpoint injecting code execution as result of rules
(define (dl_fixpoint)
  (set-idb! (make-hashtable))
  (dl_fixpoint_iterate))

(define (dl_fixpoint_iterate)
  (let* ((facts (map dl_apply_rule (get-rdb)))
         (factset (foldl (lambda (x y) (set-extend! y x)) facts (make-hashtable)))
         (new (hashtable-keys (set-difference factset (get-idb)))))
    (set-extend! (get-idb) new)
    (for-each dl_update_indices new)
    ; result of dl_apply_rule should be a tuple (this 'code (proc . args))
    (for-each (lambda (c)
      (let ((this (car c))
            (proc (caaddr c))
            (args (cdaddr c)))
         (apply proc this args))) new)
    (if (not (null? new)) (dl_fixpoint_iterate))))

(define dynamicland (append-child! (get-element-by-id "table") (make-element "div")))
(set-attribute! dynamicland "id" "dynamicland")

(define *pages* '())
(define *procs* (make-hashtable))
(define *divs* (make-hashtable))

(define (add-page proc)
  (let ((pid (dl_record 'page ('code proc))))
    (set! *pages* (cons pid *pages*))
    (hashtable-set! *procs* pid proc)
    (hashtable-set! *divs* pid (make-page-div pid)) pid))

(define (make-page-div id)
  (let ((div (make-element "div")))
    (hashtable-set! *divs* id div)
    (set-attribute! div "class" "page")
    (set-attribute! div "id" (number->string id))
    (make-div-draggable div)
    (append-child! dynamicland div) div))

(define *mouse-down* #f)
(define *mouse-offset-x* 0)
(define *mouse-offset-y* 0)

(define (make-div-draggable div)
  (add-event-listener! div "mousedown" (procedure->external (lambda (e)
    (set-z-index! div "1")
    (set! *mouse-offset-x* (- (offset-left div) (mouse-x e)))
    (set! *mouse-offset-y* (- (offset-top div) (mouse-y e)))
    (set! *mouse-down* #t))))
  (add-event-listener! div "mouseup" (procedure->external (lambda (e)
    (set-z-index! div "0")
    (recalculate-pages)	; for now, only recalculate after having dragged a page
    (set! *mouse-down* #f))))
  (add-event-listener! div "mousemove" (procedure->external (lambda (e)
    (prevent-default e)
    (if *mouse-down* (begin
      ; too slow! todo: check when page moves in/out of table bounds
      ;(recalculate-pages)
      (set-z-index! div "1")
      (set-style-left! div (format #f "~apx" (+ (mouse-x e) *mouse-offset-x*)))
      (set-style-top! div (format #f "~apx" (+ (mouse-y e) *mouse-offset-y*))))))))
; duplicate for touch events
  (add-event-listener! div "touchstart" (procedure->external (lambda (e)
    (set-z-index! div "1")
    (set! *mouse-offset-x* (- (offset-left div) (mouse-x (first-touch e))))
    (set! *mouse-offset-y* (- (offset-top div) (mouse-y (first-touch e))))
    (set! *mouse-down* #t))))
  (add-event-listener! div "touchend" (procedure->external (lambda (e)
    (set-z-index! div "0")
    (recalculate-pages)	; for now, only recalculate after having dragged a page
    (set! *mouse-down* #f))))
  (add-event-listener! div "touchmove" (procedure->external (lambda (e)
    (prevent-default e)
    (if *mouse-down* (begin
      ; too slow! todo: check when page moves in/out of table bounds
      ;(recalculate-pages)
      (set-z-index! div "1")
      (set-style-left! div (format #f "~apx" (+ (mouse-x (first-touch e)) *mouse-offset-x*)))
      (set-style-top! div (format #f "~apx" (+ (mouse-y (first-touch e)) *mouse-offset-y*)))))))))

(define (reset-page-style! pagediv)
  (let ((left (get-left pagediv))
        (top (get-top pagediv))
        (z (get-z-index pagediv)))
    (set-style! pagediv "")
    (set-style-left! pagediv left)
    (set-style-top! pagediv top)
    (set-z-index! pagediv z)
))

; When a page is in view, its code is executed. Then when all pages have ran, dl_fixpoint runs all consequences.
(define (recalculate-pages)
  (for-each (lambda (pid) (reset-page-style! (hashtable-ref *divs* pid #f))) *pages*)
  (dl_reset)
  (for-each execute-page (filter on-table? *pages*))
  (dl_fixpoint))

(define (execute-page pid)
  ((hashtable-ref *procs* pid #f) pid))

(define chInPx
  (let ((div (make-element "div")))
    (set-style! div "width:1ch;visibility:hidden")
    (append-child! (document-body) div)
    (let ((width (get-width (get-bounding-client-rect div))))
      (remove-element div) width)))

; hardcoded for now. table is 60x30ch, page is 10x10ch
(define (on-table? pid)
  (let* ((div (hashtable-ref *divs* pid #f))
         (stylex (get-left div))
         (styley (get-top div))
         (divx (string->number (substring stylex 0 (- (string-length stylex) 2)))) ;-ch
         (divy (string->number (substring styley 0 (- (string-length styley) 2)))) ;-ch
         (rect (get-bounding-client-rect dynamicland))
         (rectx (get-x rect))
         (recty (get-y rect)))
    (and (> divx rectx)
         (< (+ divx (* chInPx 10)) (+ rectx (* chInPx 60)))
         (> divy recty)
         (< (+ divy (* chInPx 10)) (+ recty (* chInPx 30))))))

(define (get-page pid)
  (hashtable-ref *divs* pid #f))

(define page1 (add-page (lambda (this) 
  (claim this 'highlighted "red"))))

(define page2 (add-page (lambda (this) 
  ; confusing: conditions need logic vars to be unquoted, code does _not_
  (when ((highlighted ,?p ,?color)) do (set-background! (get-page ?p) ?color)))))

(define page1div (get-page page1))
(define page2div (get-page page2))
(define (add-text pagediv text)
  (let ((div (make-element "div")))
    (append-child! div (make-text-node text))
    (append-child! pagediv div)))
(add-text page1div "(claim this 'highlighted \"red\")")
(add-text page2div "(when ((highlighted ,?p ,?color)) do (set-background! (get-page ?p) ?color))")

(set-style-left! page1div "10px")
(set-style-left! page2div "10px")
(set-style-top! page1div "10px")
(set-style-top! page2div "320px")

(recalculate-pages)
