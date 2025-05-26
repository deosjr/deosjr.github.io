(use-modules (scheme base)
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
     (mK-reify (take-all (call/empty-state (fresh (x ...) g0 g ...))))))) ;)

;(display (run* (x) (disj (equalo x 5) (equalo x 6))))

; javascript helpers
(define-foreign document-body
    "document" "body"
    -> (ref null extern))
(define-foreign make-text-node
    "document" "createTextNode"
    (ref string) -> (ref null extern))
(define-foreign append-child!
    "element" "appendChild"
    (ref null extern) (ref null extern) -> (ref null extern))

(define test (run* (x) (disj (equalo x 5) (equalo x 6))))
(define out (format #f "~a" test))
(append-child! (document-body) (make-text-node out))

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

(define-syntax dl_find
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

  (syntax-case stx (where)
    ((_ x where match ... )
     (let* ((datum (syntax->datum #'(x match ...)))
            (vars (remove-duplicates (collect-vars datum)))
            (gens (generate-temporaries vars))
            (sym->gen (map cons vars gens))
            (replaced-x (replace-symbols (syntax->datum #'x) sym->gen))
            (replaced-match (replace-symbols (syntax->datum #'(match ...)) sym->gen)))
           #`(run* (q #,@gens) (equalo q `#,replaced-x) (dl_findo #,(replace-symbols (syntax->datum #'(match ... )) sym->gen)) ))))))

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

(dl_record 'car ('speed 4) ('smth 5))
(define test2 (dl_find ,?y where (,?x (car speed) 4) (,?x (car smth) ,?y) ))
(define out2 (format #f "~a" test2))
(append-child! (document-body) (make-text-node out2)) ; (5)
