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
       (with-syntax ((this (datum->syntax stx 'this))
                     (dl (datum->syntax stx 'dl)))
         #'(begin
             (dl-assert! dl this 'claims (list id attr value))
             (dl-assert! dl id attr value)))))))

(define-syntax wish
  (lambda (stx)
    (syntax-case stx ()
      ((_ x)
       (with-syntax ((this (datum->syntax stx 'this))
                     (dl (datum->syntax stx 'dl)))
       #'(dl-assert! dl this 'wishes 'x))))))

#|
(define-syntax when
  (lambda (stx)
    (syntax-case stx (wishes do)
    ((_ (condition ...) do statement ... )
       (with-syntax ((this (datum->syntax stx 'this)))
           #'(dl-rule (code this (lambda (this) (begin statement ...))) :- condition ...)))
    ((_ someone wishes w do statement ... )
       (with-syntax ((this (datum->syntax stx 'this)))
           #'(dl-rule (code this (lambda (this) (begin statement ...))) :- (wishes someone w) ))))))
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
       (with-syntax ((this (datum->syntax stx 'this))
                     (dl (datum->syntax stx 'dl)))
         (let* ((datums (syntax->datum #'((cx condition cy) ...)))
            (vars (remove-duplicates (collect-vars datums)))
            (numvars (+ 1 (length vars)))
            (gens (generate-temporaries vars))
            (sym->gen (map cons vars gens))
            (st-datums (syntax->datum #'(statement ...)))
            (st-replaced (replace-symbols st-datums sym->gen))
            (replaced (replace-symbols datums sym->gen)))
       #`(dl-assert-rule! dl
           (let ((code `,(lambda (this #,@gens) (begin #,@st-replaced))))
             (fresh-vars #,numvars (lambda (q #,@gens) (conj (equalo q (list this 'code (cons code (list #,@gens)))) (dl-findo dl #,replaced) )))))))))))

; redefine dl-fixpoint! injecting code execution as result of rules
(define dl (make-new-datalog))

(define (dl-fixpoint! dl)
  (set-datalog-idb! dl (make-hashtable))
  (dl-fixpoint-iterate dl))

(define (dl-fixpoint-iterate dl)
  (let* ((facts (map (lambda (rule) (dl-apply-rule dl rule)) (datalog-rdb dl)))
         (factset (foldl (lambda (x y) (set-extend! y x)) facts (make-hashtable)))
         (new (hashtable-keys (set-difference factset (datalog-idb dl)))))
    (set-extend! (datalog-idb dl) new)
    (for-each (lambda (fact) (dl-update-indices! dl fact)) new)
    ; result of dl_apply_rule should be a tuple (this 'code (proc . args))
    (for-each (lambda (c)
      (let ((this (car c))
            (proc (caaddr c))
            (args (cdaddr c)))
         (apply proc this args))) new)
    (if (not (null? new)) (dl-fixpoint-iterate dl))))

(define dynamicland (append-child! (get-element-by-id "table") (make-element "div")))
(set-attribute! dynamicland "id" "dynamicland")

(define papers (get-element-by-id "papers"))

(define *pages* '())
(define *procs* (make-hashtable))
(define *divs* (make-hashtable))

(define (add-page proc)
  (let ((pid (dl-record! dl 'page ('code proc))))
    (set! *pages* (cons pid *pages*))
    (hashtable-set! *procs* pid proc)
    (hashtable-set! *divs* pid (make-page-div pid)) pid))

(define (make-page-div id)
  (let ((div (make-element "div")))
    (hashtable-set! *divs* id div)
    (set-attribute! div "class" "page")
    (set-attribute! div "id" (number->string id))
    (make-div-draggable div)
    (append-child! papers div) div))

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

; When a page is in view, its code is executed. Then when all pages have ran, dl-fixpoint runs all consequences.
(define (recalculate-pages)
  (for-each (lambda (pid) (reset-page-style! (hashtable-ref *divs* pid #f))) *pages*)
  (set! dl (make-new-datalog)) ; start with a fresh datalog instance
  (for-each execute-page (filter on-table? *pages*))
  (dl-fixpoint! dl))

(define (execute-page pid)
  ((hashtable-ref *procs* pid #f) pid))

(define (on-table? pid)
  (let* ((div (hashtable-ref *divs* pid #f))
         (div-rect (get-bounding-client-rect div))
         (divx (get-x div-rect))
         (divy (get-y div-rect))
         (div-width (get-width div-rect))
         (div-height (get-height div-rect))
         (table-rect (get-bounding-client-rect dynamicland))
         (tablex (get-x table-rect))
         (tabley (get-y table-rect))
         (table-width (get-width table-rect))
         (table-height (get-height table-rect)))
    (and (> divx tablex)
         (< (+ divx div-width) (+ tablex table-width))
         (> divy tabley)
         (< (+ divy div-height) (+ tabley table-height)))))

(define (get-page pid)
  (hashtable-ref *divs* pid #f))

(define *testvar* 0)

(define page1 (add-page (lambda (this) 
  ; this is an example of doing state over time the _wrong_ way, but it does work
  ;(set! *testvar* (+ 1 *testvar*))
  ;(append-child! (get-page this) (make-text-node (format #f "~a" *testvar*)))
  ; the right way would be to declare *testvar* in this scope, but that currently _doesnt_ work
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

(recalculate-pages)

(define *i* 1)
(for-each (lambda (p) 
  (let* ((page (get-page p))
         (left-string (get-left page))
         (str-len (string-length left-string))
         (left (if (= str-len 0) 0
                 (string->number (substring left-string 0 (- str-len 2)))))
         (newleft (format #f "~apx" (+ left (* *i* 50)))))
    (set! *i* (+ *i* 1))
    (set-style-left! page newleft))) *pages*)
