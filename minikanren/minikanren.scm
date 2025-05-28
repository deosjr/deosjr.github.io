(use-modules (scheme base)
             (hoot gensym)
             (hoot hashtables)
             (hoot ffi))

(define (var x) (vector x))
(define (var? x) (vector? x))
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

; walk needs assp, but only cares about true/false
; Hoot has assq and assv, but no assp!
(define assp (lambda (proc list) 
   (if (null? list) #f 
     (let ((x (car list)))
       (cond
         ((proc (car x)) x)
         (else (assp proc (cdr list))))))))

(define (walk u s)
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

(define (ext-s x v s) `((,x . ,v) . ,s))

(define (equalo u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(define (unit s/c) (cons s/c mzero))
(define mzero '())

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else (and (eqv? u v) s)))))

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

(define (mplus s1 s2)
  (cond
    ((null? s1) s2)
    ((procedure? s1) (lambda () (mplus s2 (s1))))
    (else (cons (car s1) (mplus s2 (cdr s1))))))

(define (bind s g)
  (cond
    ((null? s) mzero)
    ((procedure? s) (lambda () (bind (s) g)))
    (else (mplus (g (car s)) (bind (cdr s) g)))))

(define (pull s) 
  (if (procedure? s) (pull (s)) s))
(define (take-all s)
   (let ((s (pull s)))
     (if (null? s) '() (cons (car s) (take-all (cdr s))))))
(define (take n s)
   (if (= n 0) '()
     (let ((s (pull s)))
       (cond
         ((null? s) '())
         (else (cons (car s) (take (- n 1) (cdr s))))))))

(define-syntax zzz
   (syntax-rules ()
     ((_ g) (lambda (s/c) (lambda () (g s/c))))))

(define-syntax conj+
   (syntax-rules ()
     ((_ g) (zzz g))
     ((_ g0 g ...) (conj (zzz g0) (conj+ g ...)))))

(define-syntax disj+
   (syntax-rules ()
     ((_ g) (zzz g))
     ((_ g0 g ...) (disj (zzz g0) (disj+ g ...)))))

(define-syntax conde
   (syntax-rules ()
     ((_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...))))

(define-syntax fresh
   (syntax-rules ()
     ((_ () g0 g ...) (conj+ g0 g ...))
     ((_ (x0 x ...) g0 g ...)
      (call/fresh (lambda (x0) (fresh (x ...) g0 g ...))))))

; generalized call/fresh as a function
(define (fresh-vars count kont)
  (lambda (s/c)
    (let loop ((n count) (vars '()) (st s/c))
      (if (zero? n)
        ((apply kont (reverse vars)) st)
        (let ((c (cdr st)))
          (loop (- n 1) (cons (var c) vars) `(,(car s/c) . ,(+ c 1))) )))))

(define (mK-reify s/c*) (map reify-state/1st-var s/c*))
(define (reify-state/1st-var s/c)
   (let ((v (walk* (var 0) (car s/c))))
     (walk* v (reify-s v '()))))
(define (reify-s v s)
   (let ((v (walk v s)))
     (cond
       [(var? v) 
        (let ((n (reify-name (length s))))
          (cons (cons v n) s))]
       [(pair? v) (reify-s (cdr v) (reify-s (car v) s))]
       [else s])))
(define (reify-name n)
   (string->symbol
     (string-append "_" "." (number->string n))))
(define (walk* v s)
   (let ((v (walk v s)))
     (cond
       [(var? v) v]
       [(pair? v) (cons (walk* (car v) s) (walk* (cdr v) s))]
       [else v])))

(define empty-state (cons '() 0))
(define (call/empty-state g) (g empty-state))

(define-syntax run
  (syntax-rules ()
    ((_ n (x ...) g0 g ...)
     (mK-reify (take n (call/empty-state (fresh (x ...) g0 g ...)))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (mK-reify (take-all (call/empty-state (fresh (x ...) g0 g ...)))))))

(define (runf* goal) (mK-reify (take-all (call/empty-state goal))))

;(display (run* (x) (disj (equalo x 5) (equalo x 6))))

;(define test (runf* (fresh-vars 2 (lambda (q x) (conj (equalo q x) (disj (equalo x 5) (equalo x 6)))))))
;(define out (format #f "~a" test))
;(append-child! (document-body) (make-text-node out))

(define dl_edb (make-hashtable))
(define dl_idb (make-hashtable))
(define dl_rdb '())
(define dl_idx_entity (make-hashtable))
(define dl_idx_attr (make-hashtable))

(define dl_counter 0)
(define (dl_nextID)
   (set! dl_counter (+ dl_counter 1))
   dl_counter)

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

; javascript helpers
(define-foreign document-body
    "document" "body"
    -> (ref null extern))
(define-foreign get-element-by-id
    "document" "getElementById"
    (ref string) -> (ref null extern))
(define-foreign make-text-node
    "document" "createTextNode"
    (ref string) -> (ref null extern))
(define-foreign append-child!
    "element" "appendChild"
    (ref null extern) (ref null extern) -> (ref null extern))
(define-foreign make-element
    "document" "createElement"
    (ref string) -> (ref null extern))
(define-foreign set-attribute!
    "element" "setAttribute"
    (ref null extern) (ref string) (ref string) -> none)
(define-foreign set-background!
    "element" "setBackground"
    (ref null extern) (ref string) -> none)

(define page1func (lambda (this) 
  (claim this 'highlighted "red")
))
(define page1 (dl_record 'page ('id 1) ('code page1func)))

(define page2func (lambda (this) 
  ; confusing: conditions need logic vars to be unquoted, code does _not_
  (when ((highlighted ,?p ,?color)) do (set-background! (get-element-by-id (number->string ?p)) ?color))
))
(define page2 (dl_record 'page ('id 2) ('code page2func)))

(define dynamicland (append-child! (document-body) (make-element "div")))
(set-attribute! dynamicland "id" "dynamicland")

(define (make-page-div id)
  (let ((div (make-element "div")))
    (set-attribute! div "class" "page")
    (set-attribute! div "id" (number->string id))
    (append-child! dynamicland div) div))

(append-child! (make-page-div page1) (make-text-node "(claim this 'highlighted \"red\")"))
(append-child! (make-page-div page2) (make-text-node "(when ((highlighted ,?p ,?color)) do (set-background! (get-element-by-id (number->string ?p)) ?color))"))

; When a page is in view, its code is executed. Then when all pages have ran, dl_fixpoint runs all consequences.
(page1func page1)
(page2func page2)
(dl_fixpoint)
