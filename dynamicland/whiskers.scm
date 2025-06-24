(use-modules (dom js)
             (datalog)
             (realtalk)
             (hoot hashtables))

(define pages (get-element-by-id "pages"))

(define page1 (add-page (make-page-code
  (Wish this 'has-whiskers #t)
  ; declare what happens when pointing at a page, ie maybe color it
  (When ((points-at ,this ,?p)) do (set-background! (get-page ?p) "red"))
)))

(define page2 (add-page (make-page-code
  ; workaround (see below)
  (define (claim-has-whiskers p)
    (Claim p 'has-whiskers #t))

  ; fulfill a page's wish to have whiskers
  (When ((wishes ,?p (,?p has-whiskers ,#t))) do
    (claim-has-whiskers ?p))

  ; declare how to 'draw' whiskers ie add css class
  (When ((has-whiskers ,?p #t)) do
    (add-class! (get-page ?p) "whisker"))
)))

(define page3 (add-page (make-page-code
  ; declare what it means to point at smth
  ; makes a hard assumption on styling, i.e. whiskers extend for 50px

  ; NOTE: this is a workaround for nested macro-expansion breaking atm
  ; The hashtable-set! part is a workaround for cleaning up derived facts:
  ; There should probably be a difference between top-level Claims and nested Claims.
  ; top-level is asserted/retracted with the page entering/leaving the scene
  ; nested Claims are treated as derived facts and need to be derived every fixpoint iteration
  (define (claim-pointer-at p point)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,p pointer-at ,point)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,p pointer-at ,point) #t)
    (Claim p 'pointer-at point))
  (define (claim-point-at p q)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,p points-at ,q)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,p points-at ,q) #t)
    (Claim p 'points-at q))

  (When ((has-whiskers ,?p #t)
         ((page left) ,?p ,?x)
         ((page top) ,?p ,?y)
         ((page width) ,?p ,?width))
	; TODO: angle?
   do (let* ((w (/ ?width 2))
             (px (+ ?x w))
             (py (- ?y 50)))
         (claim-pointer-at ?p (cons px py)) ))

  ; NOTE: this fires for every page, since we can't calculate in the db atm!
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
           (claim-point-at ?p ?q))))
     ; TODO: nested Claim in When does not work!
     ; TODO: nested When in When does double macro hygiene var substitution, which will break
     ; TODO: solution could be to do all of this in a single substituting macro's scope?
     ; TODO: smth that defines a page, adds (lambda (this) ...) context and inserts 'this' everywhere?
          ;(Claim ?p 'points-at ?q))))
)))

; NOTE that using css class this way goes against Dynamicland principles in the following ways:
; - the class::before style has to be defined up front
; - the class::before style can not be modified (unless the stylesheet itself is brought into scope)

; debug page: print known facts claimed by pointed-at page next to it
(define page4 (add-page (make-page-code
  (Wish this 'has-whiskers #t)
  (When ((points-at ,this ,?p))
   do (let* ((dl (get-dl))
             (idx (datalog-idx-entity dl))
             (facts (hashtable-keys (hashtable-ref idx ?p #f)))
             (text-div (make-element "div")))
         (set-attribute! text-div "class" "text-right")
         (for-each (lambda (fact)
           (let ((p (make-element "p")))
             (append-child! p (make-text-node (format #f "~a" fact)))
             (append-child! text-div p))) facts)
         (append-child! (query-selector (get-page ?p) ".content") text-div))))))

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
