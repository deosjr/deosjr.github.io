(use-modules (dom js)
             (minikanren)
             (datalog)
             (realtalk)
             (hoot ffi)
             (hoot hashtables))

(define pages (get-element-by-id "pages"))

; todo: update whisker?
(make-dynamic)

(add-event-listener! (window) "update-realtalk" (procedure->external (lambda (e)
  (recalculate-pages))))

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
    (if pointed-at (set! pointed-at #f)))
  
)))

(define page4 (add-page (make-page-code
  (When ((time now ,?t)) do
    (let* ((mod (modulo ?t 5000))
           (x (* 360 (/ mod 5000.0))))
      (set-background! (get-page this) (format #f "hsl(~a, 100%, 50%)" x ))))
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

  (When ((has-whiskers ,?p #t)
         ((page left) ,?p ,?x)
         ((page top) ,?p ,?y)
         ((page width) ,?p ,?width))
	; TODO: angle?
   do (let* ((w (/ ?width 2))
             (px (+ ?x w))
             (py (- ?y 50)))
         (Claim ?p 'pointer-at (cons px py)) ))

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
           (Claim ?p 'points-at ?q)
           (Claim ?p 'not-points-at ?q))))
)))

(define page1div (get-page page1))
(append-child! pages page1div)
(define page2div (get-page page2))
(append-child! pages page2div)
(define page3div (get-page page3))
(append-child! pages page3div)
(define page4div (get-page page4))
(append-child! pages page4div)
(define (add-text pagediv text)
  (let ((div (make-element "div")))
    (append-child! div (make-text-node text))
    (append-child! pagediv div)))
(add-text page1div "#1")
(add-text page2div "#2")
(add-text page3div "#3")
(add-text page4div "#4")
(set-style-left! page1div "30vw")
(set-style-left! page2div "40vw")
(set-style-left! page3div "50vw")
(set-style-left! page4div "60vw")

(recalculate-pages)
