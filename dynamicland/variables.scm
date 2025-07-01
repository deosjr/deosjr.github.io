(use-modules (dom js)
             (datalog)
             (realtalk)
             (hoot hashtables))

(define pages (get-element-by-id "pages"))

(define page1 (add-page (make-page-code
  ; variables in page scope
  (define colours (list "limegreen" "cornflowerblue" "yellow" "red"))
  (define index -1)

  (When ((points-at ,?p ,this)
         (gives-colour ,?p #t)) do
    (set! index (modulo (+ index 1) (length colours)))
    (set-background! (get-page this) (list-ref colours index)))
)))

(define page2 (add-page (make-page-code
  ; variables in page scope
  (define colours (list "limegreen" "cornflowerblue" "yellow" "red"))
  (define index -1)
  (define pointed-at #f)

  ; note the semantics are still different:
  ; if any page points at, vs if any page does not point at (not 'if no page points at')
  ; they are the same currently because there is only one page pointing
  (When ((points-at ,?p ,this)
         (gives-colour ,?p #t)) do
    (if (not pointed-at)
      (set! index (modulo (+ index 1) (length colours))))
    (set! pointed-at #t)
    (set-background! (get-page this) (list-ref colours index)))
  (When ((not-points-at ,?p ,this)
         (gives-colour ,?p #t)) do
    (if pointed-at (set! pointed-at #f))
    ; bug: without this noop line, the whole thing crashes?!?!?
    ; smth smth compiler reordering and sequencing barriers ???
    ; its not actually noop, display does print to console using reflect.js but not for #t or #f
    (display pointed-at))
)))

; whiskers. see whiskers.scm
; extended to explicitly claim when _not_ pointing at a page
(define page3 (add-page (make-page-code
  (Wish this 'has-whiskers #t)
  (Claim this 'gives-colour #t)

  (define (claim-has-whiskers p)
    (Claim p 'has-whiskers #t))
  (When ((wishes ,?p (,?p has-whiskers ,#t))) do
    (claim-has-whiskers ?p))
  (When ((has-whiskers ,?p #t)) do
    (add-class! (get-page ?p) "whisker"))

  (define (claim-pointer-at p point)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,p pointer-at ,point)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,p pointer-at ,point) #t)
    (Claim p 'pointer-at point))
  (define (claim-point-at p q)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,p points-at ,q)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,p points-at ,q) #t)
    (Claim p 'points-at q))
  (define (claim-not-point-at p q)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,p not-points-at ,q)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,p not-points-at ,q) #t)
    (Claim p 'not-points-at q))

  (When ((has-whiskers ,?p #t)
         ((page left) ,?p ,?x)
         ((page top) ,?p ,?y)
         ((page width) ,?p ,?width))
	; TODO: angle?
   do (let* ((w (/ ?width 2))
             (px (+ ?x w))
             (py (- ?y 50)))
         (claim-pointer-at ?p (cons px py)) ))

  (When ((pointer-at ,?p ,?point)
         ((page left) ,?q ,?qx)
         ((page top) ,?q ,?qy)
         ((page width) ,?q ,?qw)
         ((page height) ,?q ,?qh))
	; TODO: angle?
   do (let ((px (car ?point))
            (py (cdr ?point)))
        (if (and (> px ?qx)
                 (< px (+ ?qx ?qw))
                 (> py ?qy)
                 (< py (+ ?qy ?qh)))
           (claim-point-at ?p ?q)
           (claim-not-point-at ?p ?q))))
)))

(define page1div (get-page page1))
(append-child! pages page1div)
(define page2div (get-page page2))
(append-child! pages page2div)
(define page3div (get-page page3))
(append-child! pages page3div)
(define (add-text pagediv text)
  (let ((div (make-element "div")))
    (append-child! div (make-text-node text))
    (append-child! pagediv div)))
(add-text page1div "#1")
(add-text page2div "#2")
(add-text page3div "#3")
(set-style-left! page1div "30vw")
(set-style-left! page2div "40vw")
(set-style-left! page3div "50vw")

(recalculate-pages)
