(use-modules (scheme base)
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

(define (pull s) (if (procedure? s) (pull (s)) s))
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
     (mK-reify (take-all (call/empty-state (fresh (x ...) g0 g ...)))))))

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
