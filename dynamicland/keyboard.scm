(use-modules (dom js)
             (datalog)
             (realtalk)
             (hoot hashtables)
             (hoot ffi))

(define-foreign computed-style
    "window" "getComputedStyle"
    (ref null extern) -> (ref null extern))
(define-foreign get-property
    "element" "getProperty"
    (ref null extern) (ref string) -> (ref string))
(define-foreign set-property!
    "element" "setProperty"
    (ref null extern) (ref string) (ref string) -> none)

(add-event-listener! (window) "update-realtalk" (procedure->external (lambda (e)
  (recalculate-pages))))

(define pages (get-element-by-id "pages"))

(make-dynamic)

; each loop, we assert the last key. this will only work if at most one key is pressed every 100ms (current refresh rate)
(define page1 (add-keyboard))

(define page2 (add-page (make-page-code
  (define buffer '())

  (When ((keyboard ,?p #t)
         (points-at ,?p ,this)
         (key-pressed ,?p ,?key))
    do (set! buffer (cons ?key buffer)))

  (When ((keyboard ,?p #t)
         (points-at ,?p ,this)
         ((page left) ,this ,?x)
         ((page top) ,this ,?y)
         ((page width) ,this ,?w)
         ((page height) ,this ,?h)) do
    (let* ((table-div (get-element-by-id "table"))
           (svg (query-selector table-div "svg"))
           (text (make-svg-element "text")))
      (set-inner-html! text (format #f "~a" (list->string (reverse buffer))))
      (set-attribute! text "x" (number->string (exact->inexact (+ ?x (/ ?w 2)))))
      (set-attribute! text "y" (number->string (exact->inexact (+ ?y (/ ?h 2)))))
      (set-attribute! text "style" "font: bold 60px sans-serif;fill:purple;opacity:0.8")
      (append-child! svg text)))
)))

(define page3 (add-page (make-page-code
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
(define (add-text pagediv text)
  (let ((div (make-element "div")))
    (append-child! div (make-text-node text))
    (append-child! pagediv div)))
(add-text page1div "keyboard")
(add-text page2div "")
(add-text page3div "whiskers")
(set-style-left! page1div "30vw")
(set-style-left! page2div "40vw")
(set-style-left! page3div "50vw")

(recalculate-pages)
