(use-modules (scheme base)
             (dom js)
             (minikanren)
             (hoot gensym)
             (hoot hashtables)
             (hoot ffi))

(define dl_edb (make-hashtable))
(define dl_idb (make-hashtable))
(define dl_rdb '())
(define dl_idx_entity (make-hashtable))
(define dl_idx_attr (make-hashtable))

(define dl_counter 0)
(define (dl_nextID)
   (set! dl_counter (+ dl_counter 1))
   dl_counter)

(define (dl_reset)
  (set! dl_edb (make-hashtable))
  (set! dl_idb (make-hashtable))
  (set! dl_rdb '())
  (set! dl_idx_entity (make-hashtable))
  (set! dl_idx_attr (make-hashtable))
  (set! dl_counter 0))

(define (dl_assert entity attr value)
  (hashtable-set! dl_edb (list entity attr value) #t)
  (dl_update_indices (list entity attr value)))

(define (dl_update_indices tuple)
   (let ((entity (car tuple))
         (attr (car (cdr tuple))))
     (let ((m (hashtable-ref dl_idx_entity entity #f)))
       (if m (hashtable-set! m tuple #t)
         (let ((new (make-hashtable)))
           (hashtable-set! dl_idx_entity entity new)
           (hashtable-set! new tuple #t))))
     (let ((m (hashtable-ref dl_idx_attr attr #f)))
       (if m (hashtable-set! m tuple #t)
         (let ((new (make-hashtable)))
           (hashtable-set! dl_idx_attr attr new)
           (hashtable-set! new tuple #t))))))

(define-syntax dl_record
   (syntax-rules ()
     ((_ type (attr value) ...) (let ((id (dl_nextID)))
       (dl_assert id (list type attr) value) ... id))))

; goal looks like: (fresh-vars 3 (lambda (q ?x ?y) (equalo q ?x) (dl_findo ( (,?x '(car speed) 4) ))))
(define (dl_find goal) (runf* goal))

(define-syntax dl_findo
  (syntax-rules ()
    ((_ (m ...)) (conj+ (dl_findo_ `m) ... ))))

(define (dl_findo_ m)
   (fresh (x y entity attr db)
   (conso entity x m)
   (conso attr y x)
     (conde
       [(boundo entity) (lookupo dl_idx_entity entity db) (membero m db)]
       [(unboundo entity) (boundo attr) (lookupo dl_idx_attr attr db) (membero m db)] )))

; compiles the rule to a goal function
; here we need to find the ?vars and assert #`(fresh-vars #,num-vars (lambda (#,@vars) (conj (equalo q #,head) (dl_findo #,@body))))
(define-syntax dl_rule
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

  (syntax-case stx (:-)
    ((_ (head hx hy) :- (body bx by) ...)
     (let* ((datums (syntax->datum #'((hx head hy) (bx body by) ...)))
            (head-datum (car datums))
            (body-datums (cdr datums))
            (vars (remove-duplicates (collect-vars datums)))
            (numvars (+ 1 (length vars)))
            (gens (generate-temporaries vars))
            (sym->gen (map cons vars gens))
            (replaced-head (replace-symbols head-datum sym->gen))
            (replaced-body (replace-symbols body-datums sym->gen)))
       #`(dl_assert_rule (fresh-vars #,numvars (lambda (q #,@gens) (conj (equalo q `#,replaced-head) (dl_findo #,replaced-body) )))))))))

(define (dl_assert_rule rule)
  (set! dl_rdb (cons rule dl_rdb)))

(define (dl_fixpoint)
  (set! dl_idb (make-hashtable))
  (dl_fixpoint_iterate))

(define (dl_fixpoint_iterate)
  (let* ((facts (map dl_apply_rule dl_rdb))
         (factset (foldl (lambda (x y) (set-extend! y x)) facts (make-hashtable)))
         (new (hashtable-keys (set_difference factset dl_idb))))
    (set-extend! dl_idb new)
    (for-each dl_update_indices new)
    (if (not (null? new)) (dl_fixpoint_iterate))))

(define (dl_apply_rule rule)
  (dl_find rule))

#| HELPER FUNCTIONS |#
(define (list->set x)
   (define list->set_ (lambda (a b)
     (cond
       [(null? a) b]
       [(member? b (car a)) (list->set_ (cdr a) b)]
       [else (list->set_ (cdr a) (cons (car a) b))])))
   (list->set_ x '()))

(define (foldl f l acc)
   (if (null? l) acc
     (foldl f (cdr l) (f (car l) acc))))

(define (conso a b l) (equalo (cons a b) l))
(define (boundo v)
    (lambda (s/c)
      (if (var? v)
        (let ((x (walk v (car s/c))))
          (if (var? x) mzero (unit s/c)))
        (unit s/c))))
(define (unboundo v)
    (lambda (s/c)
      (if (var? v)
        (let ((x (walk v (car s/c))))
          (if (var? x) (unit s/c) mzero))
        mzero)))
(define (lookupo m key value)
    (lambda (s/c)
      (let* ((k (if (var? key) (walk key (car s/c)) key))
             (v (hashtable-ref m k #f)))
       (if v ((equalo value (hashtable-keys v)) s/c) mzero))))

(define (membero x l)
   (fresh (a d)
     (conso a d l)
     (conde
       [(equalo a x)]
       [(membero x d)])))

(define (member? l x)
   (cond
     [(null? l) #f]
     [(eqv? (car l) x) #t]
     [else (member? (cdr l) x)]))

(define (set-extend! m keys)
   (if (null? keys) m (begin (hashtable-set! m (car keys) #t) (set-extend! m (cdr keys)))))

(define (set_difference a b)
   (define check_keys (lambda (k m)
     (if (null? k) (make-hashtable) (let ((rec (check_keys (cdr k) m)))
       (if (not (hashtable-ref m (car k) #f)) (hashtable-set! rec (car k) #t))
       rec ))))
   (check_keys (hashtable-keys a) b))

#|
;(dl_record 'car ('speed 4) ('smth 5))
; goal looks like: (fresh-vars 3 (lambda (q ?x ?y) (equalo q ?x) (dl_findo ( (,?x '(car speed) 4) ))))
;(define test2 (dl_find (fresh-vars 3 (lambda (q ?x ?y) (conj (equalo q `,?y) (dl_findo ( (,?x (car speed) 4) (,?x (car smth) ,?y) )))))))
; there could be a separate macro rewriting the below to the above
;(define test2 (dl_find ,?y where (,?x (car speed) 4) (,?x (car smth) ,?y) ))

(define a (dl_record 'vertex))
(define b (dl_record 'vertex))
(define c (dl_record 'vertex))
(define d (dl_record 'vertex))
(define e (dl_record 'vertex))
(define (dl_edge x y) (dl_assert x 'edge y))
(dl_edge a c)
(dl_edge b a)
(dl_edge b d)
(dl_edge c d)
(dl_edge d a)
(dl_edge d e)
(dl_rule (reachable ,?x ,?y) :- (edge ,?x ,?y))
(dl_rule (reachable ,?x ,?y) :- (edge ,?x ,?z) (reachable ,?z ,?y))
(dl_fixpoint)
;(define test2 (dl_find ,?id where (,?id reachable ,?id)))
(define test2 (dl_find (fresh-vars 1 (lambda (?id) (dl_findo ( (,?id reachable ,?id) ))))))

(define out2 (format #f "~a" test2)) ; expect a permutation of (1 3 4)
(append-child! (document-body) (make-text-node out2))
|#

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
(define (dl_fixpoint_iterate)
  (let* ((facts (map dl_apply_rule dl_rdb))
         (factset (foldl (lambda (x y) (set-extend! y x)) facts (make-hashtable)))
         (new (hashtable-keys (set_difference factset dl_idb))))
    (set-extend! dl_idb new)
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

; hardcoded for now. table is 800x500px, page is 300x300
(define (on-table? pid)
  (let* ((div (hashtable-ref *divs* pid #f))
         (stylex (get-left div))
         (styley (get-top div))
         (divx (string->number (substring stylex 0 (- (string-length stylex) 2)))) ;-px
         (divy (string->number (substring styley 0 (- (string-length styley) 2)))) ;-px
         (rect (get-bounding-client-rect dynamicland))
         (rectx (get-x rect))
         (recty (get-y rect)))
    (and (> divx rectx)
         (< (+ divx 300) (+ rectx 800))
         (> divy recty)
         (< (+ divy 300) (+ recty 500)))))

(define (get-page pid)
  (hashtable-ref *divs* pid #f))

(define page1 (add-page (lambda (this) 
  (claim this 'highlighted "red"))))

(define page2 (add-page (lambda (this) 
  ; confusing: conditions need logic vars to be unquoted, code does _not_
  (when ((highlighted ,?p ,?color)) do (set-background! (get-page ?p) ?color)))))

(define page1div (get-page page1))
(define page2div (get-page page2))
(append-child! page1div (make-text-node "(claim this 'highlighted \"red\")"))
(append-child! page2div (make-text-node "(when ((highlighted ,?p ,?color)) do (set-background! (get-page ?p) ?color))"))

(set-style-left! page1div "800px")
(set-style-left! page2div "800px")
(set-style-top! page1div "10px")
(set-style-top! page2div "320px")

(recalculate-pages)
