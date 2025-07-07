(use-modules (dom js)
             (datalog)
             (realtalk)
             (hoot ffi)
             (hoot hashtables))

(define-foreign window
    "window" "window"
    -> (ref null extern))
(define-foreign make-svg-element
    "document" "createSVGElement"
    (ref string) -> (ref null extern))
(define-foreign computed-style
    "window" "getComputedStyle"
    (ref null extern) -> (ref null extern))
(define-foreign get-property
    "element" "getProperty"
    (ref null extern) (ref string) -> (ref string))
(define-foreign set-property!
    "element" "setProperty"
    (ref null extern) (ref string) (ref string) -> none)
(define-foreign console-log
    "console" "log"
    (ref string) -> none)

(add-event-listener! (window) "update-realtalk" (procedure->external (lambda (e)
  (recalculate-pages))))

(define pages (get-element-by-id "pages"))

(define page1 (add-page (make-page-code
  (Wish this 'has-whiskers #t)
  ; declare what happens when pointing at a page, ie maybe color it
  (When ((points-at ,this ,?p)) do (set-background! (get-page ?p) "limegreen"))
)))

(define page2 (add-page (make-page-code
  ; workaround (see below)
  (define (claim-has-whiskers p)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,p has-whiskers #t)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,p has-whiskers #t) #t)
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
; Whiskers should probably be drawn like the text-projection is done below

; debug page: print known facts claimed by pointed-at page next to it
; todo: if fixpoint ends with a claim, that claim might not be represented
; because printing text will have happened before it?
(define page4 (add-page (make-page-code
  (Wish this 'has-whiskers #t)
  (When ((points-at ,this ,?p)
         ((page left) ,?p ,?x)
         ((page top) ,?p ,?y)
         ((page width) ,?p ,?w))
   do (let* ((dl (get-dl))
             (idx (datalog-idx-entity dl))
             (facts (hashtable-keys (hashtable-ref idx ?p #f)))
             (table-div (get-element-by-id "table"))
             (text-div (make-element "div")))
         (set-attribute! text-div "class" "text-projection")
         (set-style-left! text-div (format #f "~apx" (+ ?x ?w 10)))
         (set-style-top! text-div (format #f "~apx" ?y))
         (for-each (lambda (fact)
           (let ((p (make-element "p")))
             (append-child! p (make-text-node (format #f "~a" fact)))
             (append-child! text-div p)))
               ; 'code is used in fixpoint to not run conclusions of When's twice
               (filter (lambda (f) (not (eqv? (cadr f) 'code))) facts))
         (append-child! table-div text-div))))))

; whiskers, but improved. replaces page 2 and 3
; whisker length == page height
(define page5 (add-page (make-page-code
  (define (claim-has-whiskers p)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,p has-whiskers #t)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,p has-whiskers #t) #t)
    (Claim p 'has-whiskers #t))
  (define (claim-pointer-at p point)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,p pointer-at ,point)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,p pointer-at ,point) #t)
    (Claim p 'pointer-at point))
  (define (claim-point-at p q)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,p points-at ,q)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,p points-at ,q) #t)
    (Claim p 'points-at q))

  ; ie always, because recalc overwrites table. this should move to modules/realtalk
  ; todo: table should get this once and reset inner html of the svg element only (even for text?)
  (When (((page left) ,this ,?left)) do
    (let* ((table-div (get-element-by-id "table"))
           (table-rect (get-bounding-client-rect table-div))
           (tw (get-width table-rect))
           (th (get-height table-rect))
           (svg (make-svg-element "svg")))
      (set-attribute! svg "xmlns" "http://www.w3.org/2000/svg")
      (set-attribute! svg "width" (format #f "~a" tw))
      (set-attribute! svg "height" (format #f "~a" th))
      (append-child! table-div svg)))

  (define pi 3.1415926536)
  (define (css-deg->radians deg)
    (let* ((adjusted (- 450 deg))
           (wrapped (modulo adjusted 360))
           (radians (* wrapped (/ pi 180))))
      radians))
  (define (get-px str)
    (let* ((substr (substring str 0 (- (string-length str) 2))))
      (string->number substr)))

  (When ((wishes ,?p (,?p has-whiskers ,#t))) do
    (claim-has-whiskers ?p))

  ; page rotates around midpoint: from there to whisker end, add halfh + whisker length
  ; todo: we could be using DOMMatrix.transformPoint instead?
  ; todo: page left/top/width/height are based on bounding box (axis aligned)?
  ; all of this logic becomes easier here if we do some of the calculations in modules/realtalk
  (When ((has-whiskers ,?p #t)
         ((page left) ,?p ,?x)
         ((page top) ,?p ,?y)
         ((page width) ,?p ,?width)
         ((page height) ,?p ,?height)
         ((page rotation) ,?p ,?degrees))
   do (let* ((table-div (get-element-by-id "table"))
             (table-rect (get-bounding-client-rect table-div))
             (tw (get-width table-rect))
             (th (get-height table-rect))
             (svg (query-selector table-div "svg"))
             (line (make-svg-element "line"))
             (width (get-px (get-property (computed-style (get-page ?p)) "width" )))
             (height (get-px (get-property (computed-style (get-page ?p)) "height" )))
             (halfw (/ width 2))
             (halfh (/ height 2))
             (rad (css-deg->radians ?degrees))
             (mx (+ ?x (/ ?width 2)))
             (my (+ ?y (/ ?height 2)))
             (startx (+ mx (* halfh (cos rad))))
             (starty (- my (* halfh (sin rad))))
             (endx (+ mx (* (+ halfh height) (cos rad))))
             (endy (- my (* (+ halfh height) (sin rad)))))
         (append-child! svg line)
         (set-attribute! line "x1" (number->string startx))
         (set-attribute! line "y1" (number->string starty))
         (set-attribute! line "x2" (number->string endx))
         (set-attribute! line "y2" (number->string endy))
         (set-attribute! line "stroke" "green")
         (set-attribute! line "stroke-width" "2")
         (claim-pointer-at ?p (cons endx endy))))

  ; NOTE: this fires for every page, since we can't calculate in the db atm!
  ; todo: rotated page now checks bounding box, not actual div dimensions
  (When ((pointer-at ,?p ,?point)
         ((page left) ,?q ,?qx)
         ((page top) ,?q ,?qy)
         ((page width) ,?q ,?qw)
         ((page height) ,?q ,?qh)
	; TODO: angle of targeted page?
         ((page rotation) ,?q ,?qr))
   do (let ((px (car ?point))
            (py (cdr ?point)))
        (if (and (> px ?qx)
                 (< px (+ ?qx ?qw))
                 (> py ?qy)
                 (< py (+ ?qy ?qh)))
           (claim-point-at ?p ?q))))
)))

(define page1div (get-page page1))
(append-child! pages page1div)
(define page2div (get-page page2))
(append-child! pages page2div)
(define page3div (get-page page3))
(append-child! pages page3div)
(define page4div (get-page page4))
(append-child! pages page4div)
(define page5div (get-page page5))
(append-child! pages page5div)
(define (add-text pagediv text)
  (let ((div (make-element "div")))
    (append-child! div (make-text-node text))
    (append-child! pagediv div)))
(add-text page1div "#1")
(add-text page2div "#2")
(add-text page3div "#3")
(add-text page4div "#4")
(add-text page5div "#5")
(set-style-left! page1div "30vw")
(set-style-left! page2div "40vw")
(set-style-left! page3div "50vw")
(set-style-left! page4div "60vw")
(set-style-left! page5div "70vw")

(recalculate-pages)
