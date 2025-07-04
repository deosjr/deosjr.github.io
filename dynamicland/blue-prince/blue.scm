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

; more notes: input can be handled like the folk computer 'button' origami
; box can be a generic page with its init state 'set' by another pointing at it?

; puzzle state: ( ( a1 a2 a3 )
;                 ( b1 b2 b3 )
;                 ( c1 c2 c3 ) )
; letters when symbols/logic vars, coordinates (1 . 1) etc when not

(define pages (get-element-by-id "pages"))

; first sanctum puzzle
(define orinda (add-page (make-page-code
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
  ; TIL: named let
  (define state (make-hashtable))

  (let loop-rows ((row init) (y 1))
    (when (pair? row)
      (let loop-cols ((col (car row)) (x 1))
        (when (pair? col)
          (hashtable-set! state (cons x y) (car col))
          (loop-cols (cdr col) (+ x 1))))
      (loop-rows (cdr row) (+ y 1)))) 

  (define (state-list)
    (let row-loop ((y 1) (rows '()))
      (if (> y 3)
          (reverse rows)
          (let col-loop ((x 1) (cols '()))
            (if (> x 3)
                (row-loop (+ y 1) (cons (reverse cols) rows))
                (col-loop (+ x 1) (cons (hashtable-ref state (cons x y) #f) cols)))))))

  (Claim this 'mora-jai solution)

  ; claim solved state?
  (define (claim-state)
    ; we know this runs each iteration at the start
    (set! pointed-at-prev pointed-at)
    (set! pointed-at #f)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,this was-pointed-at ,pointed-at-prev)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,this was-pointed-at ,pointed-at-prev) #t)
    (Claim this 'was-pointed-at pointed-at-prev)
    (let ((state (state-list)))
      (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,this mora-jai-state ,state)) #t)
      (hashtable-set! (datalog-idb (get-dl)) `(,this mora-jai-state ,state) #t)
      (Claim this 'mora-jai-state state)))

  ; i.e. forever, but conditional claims since our state will change
  (When ((mora-jai ,this ,?sol)) do
    (claim-state))

  (When ((points-at ,?p ,?button)
         (button ,?button (,this ,?x ,?y ,?color)))
   do (set! pointed-at #t))

  (When ((wishes ,?p (,this updates (,?x ,?y ,?color)))) 
   do (hashtable-set! state (cons ?x ?y) ?color))

  (When ((is-solved ,this #t))
   do (set! solved #t))
)))

; engine (offload as much logic as possible from a page without keeping its state)
(define engine (add-page (make-page-code
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
(define whiskers (add-page (make-page-code
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

(define blackpage (add-page (make-page-code
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
         (was-pointed-at ,?p #f)
         (button ,?button (,?p ,?x ,?y black)))
   do (cond
        [(= ?y 1) (rotate ?p ?y ?a1 ?a2 ?a3)]
        [(= ?y 2) (rotate ?p ?y ?b1 ?b2 ?b3)]
        [(= ?y 3) (rotate ?p ?y ?c1 ?c2 ?c3)]))
)))

(define greenpage (add-page (make-page-code
  (define (wish-updates page updates)
    (for-each (lambda (update) 
      (hashtable-set! (datalog-idb (get-dl)) `(,this wishes (,page updates ,update)) #t)
      (Wish page 'updates update))
    updates))

  (When ((mora-jai-state ,?p ((,?a1 ,?a2 ,?a3)
                              (,?b1 ,?b2 ,?b3)
                              (,?c1 ,?c2 ,?c3)))
         (points-at ,?page ,?button)
         (was-pointed-at ,?p #f)
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

(define yellowpage (add-page (make-page-code
  (define (wish-updates page updates)
    (for-each (lambda (update) 
      (hashtable-set! (datalog-idb (get-dl)) `(,this wishes (,page updates ,update)) #t)
      (Wish page 'updates update))
    updates))

  (When ((mora-jai-state ,?p ((,?a1 ,?a2 ,?a3)
                              (,?b1 ,?b2 ,?b3)
                              (,?c1 ,?c2 ,?c3)))
         (points-at ,?page ,?button)
         (was-pointed-at ,?p #f)
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

(define orangepage (add-page (make-page-code
  (define (wish-updates page updates)
    (for-each (lambda (update) 
      (hashtable-set! (datalog-idb (get-dl)) `(,this wishes (,page updates ,update)) #t)
      (Wish page 'updates update))
    updates))

  (define (count-color color lst)
    (length (filter (lambda (x) (equal? x color)) lst)))

  (define (majority-color? colors)
    (define half (/ (length colors) 2.0))
    (define (check lst)
      (cond
        ((null? lst) #f)
        ((> (count-color (car lst) colors) half) (car lst))
        (else (check (cdr lst)))))
    (check colors))

  (define (switch-if-majority page x y neighbours)
    (let ((majority (majority-color? neighbours)))
      (if majority (wish-updates page `((,x ,y ,majority)) ))))

  (When ((mora-jai-state ,?p ((,?a1 ,?a2 ,?a3)
                              (,?b1 ,?b2 ,?b3)
                              (,?c1 ,?c2 ,?c3)))
         (points-at ,?page ,?button)
         (was-pointed-at ,?p #f)
         (button ,?button (,?p ,?x ,?y orange)))
   do (cond
        [(and (= ?x 1) (= ?y 1))
           (switch-if-majority ?p ?x ?y (list ?b1 ?a2))]
        [(and (= ?x 2) (= ?y 1))
           (switch-if-majority ?p ?x ?y (list ?a1 ?b2 ?a3))]
        [(and (= ?x 3) (= ?y 1))
           (switch-if-majority ?p ?x ?y (list ?a2 ?b3))]
        [(and (= ?x 1) (= ?y 2))
           (switch-if-majority ?p ?x ?y (list ?a1 ?b2 ?c1))]
        [(and (= ?x 2) (= ?y 2))
           (switch-if-majority ?p ?x ?y (list ?a2 ?b1 ?b3 ?c2))]
        [(and (= ?x 3) (= ?y 2))
           (switch-if-majority ?p ?x ?y (list ?a3 ?b2 ?c3))]
        [(and (= ?x 1) (= ?y 3))
           (switch-if-majority ?p ?x ?y (list ?b1 ?c2))]
        [(and (= ?x 2) (= ?y 3))
           (switch-if-majority ?p ?x ?y (list ?c1 ?b2 ?c3))]
        [(and (= ?x 3) (= ?y 3))
           (switch-if-majority ?p ?x ?y (list ?c2 ?b3))]))
)))

; third sanctum puzzle
(define archaries (add-page (make-page-code
  (define init '((black yellow  gray)
                 (yellow green  yellow)
                 (gray yellow   black)))
  (define solution 'black)

  (Claim this 'mora-jai solution)

  (define solved #f)
  (define pointed-at #f)
  (define pointed-at-prev #f)

  (define state (make-hashtable))

  (let loop-rows ((row init) (y 1))
    (when (pair? row)
      (let loop-cols ((col (car row)) (x 1))
        (when (pair? col)
          (hashtable-set! state (cons x y) (car col))
          (loop-cols (cdr col) (+ x 1))))
      (loop-rows (cdr row) (+ y 1)))) 

  (define (state-list)
    (let row-loop ((y 1) (rows '()))
      (if (> y 3)
          (reverse rows)
          (let col-loop ((x 1) (cols '()))
            (if (> x 3)
                (row-loop (+ y 1) (cons (reverse cols) rows))
                (col-loop (+ x 1) (cons (hashtable-ref state (cons x y) #f) cols)))))))

  (define (claim-state)
    ; we know this runs each iteration at the start
    (set! pointed-at-prev pointed-at)
    (set! pointed-at #f)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,this was-pointed-at ,pointed-at-prev)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,this was-pointed-at ,pointed-at-prev) #t)
    (Claim this 'was-pointed-at pointed-at-prev)
    (let ((state (state-list)))
      (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,this mora-jai-state ,state)) #t)
      (hashtable-set! (datalog-idb (get-dl)) `(,this mora-jai-state ,state) #t)
      (Claim this 'mora-jai-state state)))

  ; i.e. forever, but conditional claims since our state will change
  (When ((mora-jai ,this ,?sol)) do
    (claim-state))

  (When ((points-at ,?p ,?button)
         (button ,?button (,this ,?x ,?y ,?color)))
   do (set! pointed-at #t))

  (When ((wishes ,?p (,this updates (,?x ,?y ,?color)))) 
   do (hashtable-set! state (cons ?x ?y) ?color))

  (When ((is-solved ,this #t))
   do (set! solved #t))
)))

(define (add-text pagediv text)
  (let ((div (make-element "div")))
    (append-child! div (make-text-node text))
    (append-child! pagediv div)))

(define orinda-div (get-page orinda))
(add-text orinda-div "orinda aries")
(append-child! pages orinda-div)

(define engine-div (get-page engine))
(append-child! pages engine-div)
(add-text engine-div "engine")

(define whiskers-div (get-page whiskers))
(append-child! pages whiskers-div)
(add-text whiskers-div "whiskers")

(define blackpage-div (get-page blackpage))
(append-child! pages blackpage-div)
(add-text blackpage-div "black")

(define greenpage-div (get-page greenpage))
(append-child! pages greenpage-div)
(add-text greenpage-div "green")

(define yellowpage-div (get-page yellowpage))
(append-child! pages yellowpage-div)
(add-text yellowpage-div "yellow")

(define orangepage-div (get-page orangepage))
(append-child! pages orangepage-div)
(add-text orangepage-div "orange")

(define archaries-div (get-page archaries))
(append-child! pages archaries-div)
(add-text archaries-div "arch aries")

(set-style-left! orinda-div "20vw")
(set-style-left! engine-div "30vw")
(set-style-left! whiskers-div "40vw")
(set-style-left! blackpage-div "50vw")
(set-style-left! greenpage-div "60vw")
(set-style-left! yellowpage-div "70vw")
(set-style-left! archaries-div "80vw")
(set-style-left! orangepage-div "90vw")

(recalculate-pages)
