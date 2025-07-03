(use-modules (dom js)
             (datalog)
             (realtalk)
             (hoot ffi)
             (hoot hashtables))

(define-foreign window
    "window" "window"
    -> (ref null extern))
(define-foreign console-log
    "console" "log"
    (ref string) -> none)
(define-foreign get-property
    "element" "getProperty"
    (ref null extern) (ref string) -> (ref null extern))
(define-foreign set-property!
    "element" "setProperty"
    (ref null extern) (ref string) (ref string) -> none)

(add-event-listener! (window) "update-realtalk" (procedure->external (lambda (e)
  (recalculate-pages))))

; The idea: pages can be mora-jai puzzle boxes, declaring initial position and solution conditions.
; They project nine squares and are stateful. Squares can be 'pushed' by pointing at the center (-ish).
; The core 'engine' should be a few pages tops, with multiple boxes supported simultaneously.
; Separate pages per color logic? So we can swap in/out what a color 'means'.

; Decision: engine keeps state. This maybe goes against dynamicland design?
; reason being that declaring a new box becomes a lot easier. What we lose: resetting state by removing box from table.
; The only way I see pages keeping their own state is with a lot of duplication of code per box page.
; For now, I'll keep a hashmap with box state on an engine page.

; Revert decision: pages keep state. Duplication of code should not be an issue.
; This is turning into a debate with myself about DRY principles :)
; Reasoning: in Dynamicland, pages are physical objects with code printed on them.
; Duplicating them is a simple manner of printing a new page and perhaps adjusting some parameters first.
; Changing a page means pointing a keyboard at it and updating it for the duration of it being on the table.
; Duplication is annoying when we have many similar copies and we want to update the engine.
; But is this really a problem? Perhaps we want to store our old behaviour, printing new pages is cheap.
; Adding a new color to the mora-jai puzzles does not affect old boxes, and the core logic is simple enough.
; Adding more complex solution modes (different colors) also does not affect old boxes.

; Page state could be a hashmap, and we could even pass the hashmap around in Claims.
; This feels abuse though, and is probably not what we want.
; I imagine immutable Claims will be useful when execution contexts span multiple host computers.

; Figuring out how to detect the puzzle is solved is another interesting topic.
; I started with claiming updates per button individually. 
; This would lead to solution state being detected 'halfway through' an update.
; Also, the old claimed state (mora-jai-state) clashes, which makes things hard
; If solution is checked on mora-jai-state, it is only found _next_ iteration of fixpoint
; Next I batched the updates into one big wish, and claimed a mora-jai-state-final when that was received
; This works (solution is checked on final state) but feels.. forced?
; Basically writing code that understands a lot of the internal operating mechanism instead of just declaring facts
; Solution: simplify by depending on recalculate-pages running often and automatically?

; puzzle state: ( ( a1 a2 a3 )
;                 ( b1 b2 b3 )
;                 ( c1 c2 c3 ) )
; letters when symbols/logic vars, coordinates (1 . 1) etc when not

(define pages (get-element-by-id "pages"))

; first sanctum puzzle
(define page1 (add-page (make-page-code
  (define solved #f)
  (define pointed-at #f)
  (define pointed-at-prev #f)
  (define solution 'black)
  (define init '((green black  green)
                 (black black  black)
                 (green yellow green)))

  ; state is stored as hashtable and asserted as a list
  ; this lets us pattern match but also update by coordinate
  ; and decouples page state from claimed state
  (define state (make-hashtable))
  (hashtable-set! state (cons 1 1) (car (car init)))
  (hashtable-set! state (cons 2 1) (cadr (car init)))
  (hashtable-set! state (cons 3 1) (caddr (car init)))
  (hashtable-set! state (cons 1 2) (car (cadr init)))
  (hashtable-set! state (cons 2 2) (cadr (cadr init)))
  (hashtable-set! state (cons 3 2) (caddr (cadr init)))
  (hashtable-set! state (cons 1 3) (car (caddr init)))
  (hashtable-set! state (cons 2 3) (cadr (caddr init)))
  (hashtable-set! state (cons 3 3) (caddr (caddr init)))

  (Claim this 'mora-jai solution)

  (define (claim-state)
    ; we know this runs each iteration at the start
    (set! pointed-at-prev pointed-at)
    (set! pointed-at #f)
    ; todo: not pretty, but works
    (let ((state `(
      ( ,(hashtable-ref state (cons 1 1) #f) ,(hashtable-ref state (cons 2 1) #f) ,(hashtable-ref state (cons 3 1) #f) )
      ( ,(hashtable-ref state (cons 1 2) #f) ,(hashtable-ref state (cons 2 2) #f) ,(hashtable-ref state (cons 3 2) #f) )
      ( ,(hashtable-ref state (cons 1 3) #f) ,(hashtable-ref state (cons 2 3) #f) ,(hashtable-ref state (cons 3 3) #f) )
    )))
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,this mora-jai-state ,state)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,this mora-jai-state ,state) #t)
    (Claim this 'mora-jai-state state)))

  ; i.e. forever, but conditional claims since our state will change
  (When ((mora-jai ,this ,?sol)) do
    (claim-state))

  (define (claim-newly-pointed-at)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,this newly-pointed-at #t)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,this newly-pointed-at #t) #t)
    (Claim this 'newly-pointed-at #t))

  (When ((points-at ,?p ,?button)
         (button ,?button (,this ,?x ,?y ,?color)))
   do (set! pointed-at #t)
      (if (not pointed-at-prev) (claim-newly-pointed-at)))

  (When ((wishes ,?p (,this updates (,?x ,?y ,?color)))) 
   do (hashtable-set! state (cons ?x ?y) ?color)
      ; this line needed to not make things blow up?!?
      (console-log (format #f "update ~a,~a to ~a" ?x ?y ?color)))
)))

; engine (offload as much logic as possible from a page without keeping its state)
(define page2 (add-page (make-page-code
  ; todo: define a stylesheet with a class and add to DOM? what would be cleaner?
  (define stylefmt "left:~apx;top:~apx;width:~apx;height:~apx;background:~a;position:absolute;opacity:0.7;border-style:solid;")
  ;(define stylefmt "left:~apx;top:~apx;width:~apx;height:~apx;background:~a;position:absolute;opacity:0.7;border-style:solid;align-content:center;justify-items:center;")
  ; todo: move this to the debug page?
  ;(define selectfmt "border-radius:50%;border-style:solid;width:~apx;height:~apx;color:pink")

  (define select-radius 10)
  (define button-padding 5)

  (define (claim-button page div midpoint x y color)
    (let ((args `(,page ,x ,y ,color)))
      (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,div button ,args)) #t)
      (hashtable-set! (datalog-idb (get-dl)) `(,div button ,args) #t)
      (Claim div 'button args))
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,div button-div ,midpoint)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,div button-div ,midpoint) #t)
    (Claim div 'button-div midpoint))

  (define (claim-and-draw-button page cx cy w h color x y)
    (let* ((table-div (get-element-by-id "table"))
           (div (make-element "div"))
           ;(select (make-element "div"))
           ;(diameter (* select-radius 2))
           (mx (+ x (/ w 2)))
           (my (+ y (/ h 2)))
           (mid (cons mx my)))
      (set-attribute! div "style" (format #f stylefmt x y w h (symbol->string color)))
      ;(set-attribute! select "style" (format #f selectfmt diameter diameter))
      ;(append-child! div select)
      (append-child! table-div div)
      (claim-button page div mid cx cy color)
  ))

  ; bug: not unpacking state leads to weird amounts of this rule executing
  ; perhaps depending on how many ways minikanren is able to reach this goal?
  ;(When ((mora-jai-state ,?p ,?state)) do

  ; todo: draw solution as a border around the box?
  (When ((mora-jai-state ,?p ((,?a1 ,?a2 ,?a3)
                              (,?b1 ,?b2 ,?b3)
                              (,?c1 ,?c2 ,?c3)))
         ((page left) ,?p ,?x)
         ((page top) ,?p ,?y)
         ((page width) ,?p ,?w)
         ((page height) ,?p ,?h))
   do (let* ((w (- ?w button-padding))
             (h (- ?h button-padding))
             (minx (- ?x w button-padding button-padding))
             (maxx (+ ?x w button-padding button-padding))
             (miny (- ?y h button-padding button-padding))
             (maxy (+ ?y h button-padding button-padding)))
        (claim-and-draw-button ?p 1 1 w h ?a1 minx miny)
        (claim-and-draw-button ?p 2 1 w h ?a2 ?x miny)
        (claim-and-draw-button ?p 3 1 w h ?a3 maxx miny)
        (claim-and-draw-button ?p 1 2 w h ?b1 minx ?y)
        (claim-and-draw-button ?p 2 2 w h ?b2 ?x ?y)
        (claim-and-draw-button ?p 3 2 w h ?b3 maxx ?y)
        (claim-and-draw-button ?p 1 3 w h ?c1 minx maxy)
        (claim-and-draw-button ?p 2 3 w h ?c2 ?x maxy)
        (claim-and-draw-button ?p 3 3 w h ?c3 maxx maxy)))

  ; pointing at the center of a square
  (define (euclidian px py qx qy)
    (let* ((dx (- px qx))
           (dy (- py qy)))
      (sqrt (+ (* dx dx) (* dy dy)))))

  (define (claim-point-at p q)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,p points-at ,q)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,p points-at ,q) #t)
    (Claim p 'points-at q))

  (When ((pointer-at ,?p (,?px . ,?py))
         (button-div ,?div (,?mx . ,?my))) do
    (if (< (euclidian ?px ?py ?mx ?my) select-radius) (claim-point-at ?p ?div)))

  ; update the button color immediately
  ; note that get-property is defined to return an external ref to make this work
  (When ((wishes ,?wisher (,?p updates (,?x ,?y ,?color)))
         (button ,?button (,?p ,?x ,?y ,?old)))
   do (set-property! (get-property ?button "style") "background" (symbol->string ?color)))

  ; todo: wish for p to be solved, and let p update its status + assert it each cycle?
  (define (claim-solved p)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,p is-solved #t)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,p is-solved #t) #t)
    (Claim p 'is-solved #t))

  ; note: solution is only found next iteration
  ; todo: if solution 'locks' the puzzle, is there still a race in evaluation?
  ; i.e. can an update trigger in between, locking the puzzle in an invalid state?
  (When ((mora-jai ,?p ,?sn)
         (mora-jai-state ,?p ((,?sn ,?a2 ,?sn)
                              (,?b1 ,?b2 ,?b3)
                              (,?sn ,?c2 ,?sn))))
   do (claim-solved ?p))

  (When ((is-solved ,?p #t))
   do (set-background! (get-page this) "gold"))
)))

; whiskers. doubles as a pointing device!
(define page3 (add-page (make-page-code
  (Wish this 'has-whiskers #t)

  (define (claim-has-whiskers p)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,p has-whiskers #t)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,p has-whiskers #t) #t)
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

  (When ((has-whiskers ,?p #t)
         ((page left) ,?p ,?x)
         ((page top) ,?p ,?y)
         ((page width) ,?p ,?width))
	; TODO: angle?
   do (let* ((w (/ ?width 2))
             (px (+ ?x w))
             (py (- ?y 50)))
         (claim-pointer-at ?p (cons px py)) ))

  (When ((pointer-at ,?p (,?px . ,?py))
         ((page left) ,?q ,?qx)
         ((page top) ,?q ,?qy)
         ((page width) ,?q ,?qw)
         ((page height) ,?q ,?qh))
	; TODO: angle?
   do (if (and (> ?px ?qx)
               (< ?px (+ ?qx ?qw))
               (> ?py ?qy)
               (< ?py (+ ?qy ?qh)))
         (claim-point-at ?p ?q)))
)))

(define page4 (add-page (make-page-code
  (define (wish-updates page updates)
    (for-each (lambda (update) 
      (hashtable-set! (datalog-idb (get-dl)) `(,this wishes (,page updates ,update)) #t)
      (Wish page 'updates update))
    updates))

  (define (rotate page y x1 x2 x3)
    (wish-updates page `((1 ,y ,x3) (2 ,y ,x1) (3 ,y ,x2))))

  ; todo: using ,?y instead of 1/2/3 again gives random amount of executions here when using ?color instead of black
  ; we could duplicate Whens with hardcoded y instead of using cond
  (When ((mora-jai-state ,?p ((,?a1 ,?a2 ,?a3)
                              (,?b1 ,?b2 ,?b3)
                              (,?c1 ,?c2 ,?c3)))
         (points-at ,?page ,?button)
         (newly-pointed-at ,?p #t)
         (button ,?button (,?p ,?x ,?y black)))
   do (cond
        [(= ?y 1) (rotate ?p ?y ?a1 ?a2 ?a3)]
        [(= ?y 2) (rotate ?p ?y ?b1 ?b2 ?b3)]
        [(= ?y 3) (rotate ?p ?y ?c1 ?c2 ?c3)]))
)))

(define page5 (add-page (make-page-code
  (define (wish-updates page updates)
    (for-each (lambda (update) 
      (hashtable-set! (datalog-idb (get-dl)) `(,this wishes (,page updates ,update)) #t)
      (Wish page 'updates update))
    updates))

  (When ((mora-jai-state ,?p ((,?a1 ,?a2 ,?a3)
                              (,?b1 ,?b2 ,?b3)
                              (,?c1 ,?c2 ,?c3)))
         (points-at ,?page ,?button)
         (newly-pointed-at ,?p #t)
         (button ,?button (,?p ,?x ,?y green)))
   do (cond
        [(and (= ?x 1) (= ?y 1))
           (wish-updates ?p `((3 3 green) (1 1 ,?c3)))]
        [(and (= ?x 2) (= ?y 1))
           (wish-updates ?p `((2 3 green) (2 1 ,?c2)))]
        [(and (= ?x 3) (= ?y 1))
           (wish-updates ?p `((1 3 green) (3 1 ,?c1)))]
        [(and (= ?x 1) (= ?y 2))
           (wish-updates ?p `((3 2 green) (1 2 ,?b3)))]
        [(and (= ?x 3) (= ?y 2))
           (wish-updates ?p `((1 2 green) (3 2 ,?b1)))]
        [(and (= ?x 1) (= ?y 3))
           (wish-updates ?p `((3 1 green) (1 3 ,?a3)))]
        [(and (= ?x 2) (= ?y 3))
           (wish-updates ?p `((2 1 green) (2 3 ,?a2)))]
        [(and (= ?x 3) (= ?y 3))
           (wish-updates ?p `((1 1 green) (3 3 ,?a1)))]))
)))

(define page6 (add-page (make-page-code
  (define (wish-updates page updates)
    (for-each (lambda (update) 
      (hashtable-set! (datalog-idb (get-dl)) `(,this wishes (,page updates ,update)) #t)
      (Wish page 'updates update))
    updates))

  (When ((mora-jai-state ,?p ((,?a1 ,?a2 ,?a3)
                              (,?b1 ,?b2 ,?b3)
                              (,?c1 ,?c2 ,?c3)))
         (points-at ,?page ,?button)
         (newly-pointed-at ,?p #t)
         (button ,?button (,?p ,?x ,?y yellow)))
   do (cond
        [(and (= ?x 1) (= ?y 2))
           (wish-updates ?p `((1 1 yellow) (1 2 ,?a1)))]
        [(and (= ?x 2) (= ?y 2))
           (wish-updates ?p `((2 1 yellow) (2 2 ,?a2)))]
        [(and (= ?x 3) (= ?y 2))
           (wish-updates ?p `((3 1 yellow) (3 2 ,?a3)))]
        [(and (= ?x 1) (= ?y 3))
           (wish-updates ?p `((1 2 yellow) (1 3 ,?b1)))]
        [(and (= ?x 2) (= ?y 3))
           (wish-updates ?p `((2 2 yellow) (2 3 ,?b2)))]
        [(and (= ?x 3) (= ?y 3))
           (wish-updates ?p `((3 2 yellow) (3 3 ,?b3)))]))
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
(define page6div (get-page page6))
(append-child! pages page6div)
(define (add-text pagediv text)
  (let ((div (make-element "div")))
    (append-child! div (make-text-node text))
    (append-child! pagediv div)))
(add-text page1div "orinda box")
(add-text page2div "engine")
(add-text page3div "whiskers")
(add-text page4div "black")
(add-text page5div "green")
(add-text page6div "yellow")
(set-style-left! page1div "20vw")
(set-style-left! page2div "30vw")
(set-style-left! page3div "40vw")
(set-style-left! page4div "50vw")
(set-style-left! page5div "60vw")
(set-style-left! page6div "70vw")

(recalculate-pages)
