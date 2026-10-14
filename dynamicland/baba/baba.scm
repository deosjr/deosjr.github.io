(use-modules (dom js)
             (datalog)
             (realtalk)
             (hoot ffi)
             (hoot hashtables))

(define-foreign computed-style
    "window" "getComputedStyle"
    (ref null extern) -> (ref null extern))
(define-foreign get-property
    "element" "getProperty"
    (ref null extern) (ref string) -> (ref string))

(add-event-listener! (window) "update-realtalk" (procedure->external (lambda (e)
  (recalculate-pages))))

(make-dynamic)

; Baba Is You, built from a copy of the Sokoban pages. The rules of the game are themselves
; objects on the board: text tiles that form statements like BABA IS YOU or WALL IS STOP.
;
; - level pages keep their own state (a list of objects) and claim each object as a fact
; - the words page says which words are nouns, verbs and properties
; - the grammar page lets every verb claim the rows of three it sits in the middle of,
;   and turns rows that read noun-verb-property into rules like (baba is you)
; - the physics page moves whatever is YOU, and decides when a level is won
; - the projector page draws any level it can see
; - a controls page turns key presses into move wishes on whatever level the keyboard points at
;
; Levels are written as text, using the legend on the level page:
;   things:  b baba   w wall   r rock   f flag
;   text:    B BABA   W WALL   R ROCK   F FLAG   = IS   Y YOU   ! WIN   S STOP   P PUSH
; and parsed into a list of objects (kind (x . y)) when the page lands on the table,
; where kind is a symbol like wall for things, or a list like (text wall) for text.
; Each level claims:
;   (level baba-size (columns . rows))   static, claimed once
;   (level baba-objects (object ...))    each tick
;   (level baba-thing (kind pos))        each tick, one per thing
;   (level baba-text (word pos))         each tick, one per text tile
;   (level baba-moves n)                 each tick
;   (level baba-rules (rule ...))        each tick, but only once the rules have settled (see below)
;
; Moving needs the complete set of rules, but datalog cannot collect facts into a list,
; and rules are derived at some point during a tick. So the level page collects every rule
; claimed about it during a tick, and publishes them as one list the next tick, but only
; if its state did not change in between. Moves wait in a queue until the rules are settled.
; This is the same collect-now, claim-next-tick trick as was-pointed-at in the Mora-Jai puzzles.

(define pages (get-element-by-id "pages"))
(define more-pages (get-element-by-id "more-pages"))

; Every level page has the same code apart from its starting layout.
(define (level-page rows)
  (add-page (make-page-code
    (define legend
      '((#\b . baba) (#\w . wall) (#\r . rock) (#\f . flag)
        (#\B . (text baba)) (#\W . (text wall)) (#\R . (text rock)) (#\F . (text flag))
        (#\= . (text is))
        (#\Y . (text you)) (#\! . (text win)) (#\S . (text stop)) (#\P . (text push))))

    (define (parse rows)
      (let loop-rows ((rows rows) (y 0) (acc '()))
        (if (null? rows)
            (reverse acc)
            (let loop-cols ((cs (string->list (car rows))) (x 0) (acc acc))
              (if (null? cs)
                  (loop-rows (cdr rows) (+ y 1) acc)
                  (let ((entry (assv (car cs) legend)))
                    (loop-cols (cdr cs) (+ x 1)
                               (if entry (cons (list (cdr entry) (cons x y)) acc) acc))))))))

    (define init (parse rows))
    (define size (cons (apply max (map string-length rows)) (length rows)))

    (define objects init)
    ; previous object lists, most recent first
    (define history '())
    ; once solved, the level ignores all wishes until it is taken off the table,
    ; which runs this page code again from scratch
    (define solved #f)
    ; bumped on every change to objects, so we know whether collected rules still apply
    (define version 0)
    ; move directions waiting for settled rules, oldest first
    (define pending '())
    ; rules claimed about this level so far this tick, and the version they were derived from
    (define rules '())
    (define rules-version -1)

    (define (change! next)
      (set! objects next)
      (set! version (+ version 1)))

    (Claim this 'baba-level #t)
    (Claim this 'baba-size size)

    ; runs first thing each tick, since it only depends on our own claim
    (When ((this baba-level #t)) do
      (let ((settled (= rules-version version))
            (settled-rules rules))
        (set! rules '())
        (set! rules-version version)
        (Claim this 'baba-objects objects)
        (for-each (lambda (obj)
                    (if (pair? (car obj))
                        (Claim this 'baba-text (list (cadr (car obj)) (cadr obj)))
                        (Claim this 'baba-thing obj)))
                  objects)
        (Claim this 'baba-moves (length history))
        (if solved (Claim this 'baba-solved #t))
        (if (and settled (not solved))
            (begin
              (Claim this 'baba-rules settled-rules)
              (if (pair? pending) (Claim this 'baba-move (car pending)))))))

    (When ((this baba-rule ?rule)) do
      (set! rules (cons ?rule rules)))

    (When ((?p wishes (this move ?dir))) do
      (if (not solved) (set! pending (append pending (list ?dir)))))

    ; the answer to a baba-move claim; next may be unchanged if nothing could move
    (When ((?p wishes (this set-state ?next))) do
      (if (and (not solved) (pair? pending))
          (begin
            (set! pending (cdr pending))
            (if (not (equal? ?next objects))
                (begin (set! history (cons objects history))
                       (change! ?next))))))

    (When ((?p wishes (this solved #t))) do
      (set! solved #t))

    (When ((?p wishes (this undo #t))) do
      (if (and (not solved) (pair? history))
          (begin (change! (car history))
                 (set! history (cdr history))
                 (set! pending '()))))

    (When ((?p wishes (this reset #t))) do
      (if (not solved)
          (begin (change! init)
                 (set! history '())
                 (set! pending '())))))))

; Break WALL IS STOP to get to the flag.
(define wall-is-stop (level-page
  '("B=Y.w...."
    "....w...."
    "....w..f."
    ".b..w...."
    "....w...."
    "W=S.w.F=!"
    "....w....")))

; No flag here: something else has to be WIN.
(define wall-is-win (level-page
  '("...www..."
    "...w.w..."
    "...www..."
    "W=S......"
    "...!....."
    ".b......."
    "B=Y......")))

; Baba cannot reach the flag, but something else could.
(define rock-is-you (level-page
  '("B=Y..w..."
    ".....w.r."
    ".b...w..."
    ".....w.f."
    "R....w..."
    ".=...w..."
    "Y....wF=!")))

; What the words mean. Take this page off the table and nothing parses.
(define words (add-page (make-page-code
  (for-each (lambda (entry) (Claim (car entry) 'word-class (cdr entry)))
    '((baba . noun) (wall . noun) (rock . noun) (flag . noun)
      (is . verb)
      (you . property) (win . property) (stop . property) (push . property)))
)))

(define grammar (add-page (make-page-code
  ; Any verb claims the two rows of three that it sits in the middle of.
  (When ((?level baba-text (?verb ?pos))
         (?verb word-class verb))
   do (let ((x (car ?pos))
            (y (cdr ?pos)))
        (Claim ?level 'baba-row (list (cons (- x 1) y) ?pos (cons (+ x 1) y)))
        (Claim ?level 'baba-row (list (cons x (- y 1)) ?pos (cons x (+ y 1))))))

  ; A row that reads noun-verb-property is a rule.
  (When ((?level baba-row (?first ?middle ?last))
         (?level baba-text (?subject ?first))
         (?subject word-class noun)
         (?level baba-text (?verb ?middle))
         (?verb word-class verb)
         (?level baba-text (?property ?last))
         (?property word-class property))
   do (Claim ?level 'baba-rule (list ?subject ?verb ?property))
      (Claim ?level 'baba-active-text ?first)
      (Claim ?level 'baba-active-text ?middle)
      (Claim ?level 'baba-active-text ?last))
)))

; What the rules do: YOU moves, STOP blocks, PUSH (and all text) gets pushed,
; and a level is won when something that is YOU shares a cell with something that is WIN.
(define physics (add-page (make-page-code
  (define (offset dir)
    (case dir
      ((up) '(0 . -1))
      ((down) '(0 . 1))
      ((left) '(-1 . 0))
      ((right) '(1 . 0))
      (else '(0 . 0))))

  (define (add pos d)
    (cons (+ (car pos) (car d)) (+ (cdr pos) (cdr d))))

  (define (any? pred xs)
    (and (pair? xs) (or (pred (car xs)) (any? pred (cdr xs)))))

  (define (keep pred xs)
    (cond ((null? xs) '())
          ((pred (car xs)) (cons (car xs) (keep pred (cdr xs))))
          (else (keep pred (cdr xs)))))

  (define (indices n)
    (let loop ((i (- n 1)) (acc '()))
      (if (< i 0) acc (loop (- i 1) (cons i acc)))))

  ; text is always pushable, and nothing else applies to it
  (define (is? kind property rules)
    (if (pair? kind)
        (eq? property 'push)
        (member (list kind 'is property) rules)))

  (define (won? objects rules)
    (any? (lambda (you)
            (and (is? (car you) 'you rules)
                 (any? (lambda (win)
                         (and (is? (car win) 'win rules)
                              (equal? (cadr win) (cadr you))))
                       objects)))
          objects))

  ; move everything that is YOU one step in dir, pushing what is in the way
  (define (step objects size rules dir)
    (let* ((v (list->vector objects))
           (n (vector-length v))
           (d (offset dir)))
      (define (kind i) (car (vector-ref v i)))
      (define (pos i) (cadr (vector-ref v i)))
      (define (has? i property) (is? (kind i) property rules))
      (define (at p) (keep (lambda (i) (equal? (pos i) p)) (indices n)))
      (define (inside? p)
        (and (>= (car p) 0) (< (car p) (car size))
             (>= (cdr p) 0) (< (cdr p) (cdr size))))
      (define (blocked? i)
        (let ((t (add (pos i) d)))
          (or (not (inside? t))
              (any? (lambda (j) (and (has? j 'stop) (not (has? j 'push)))) (at t))
              (any? blocked? (keep (lambda (j) (has? j 'push)) (at t))))))
      (define (move! i)
        (let ((t (add (pos i) d)))
          (for-each move! (keep (lambda (j) (has? j 'push)) (at t)))
          (vector-set! v i (list (kind i) t))))
      (for-each (lambda (i) (if (not (blocked? i)) (move! i)))
                (keep (lambda (i) (has? i 'you)) (indices n)))
      (vector->list v)))

  (When ((?level baba-move ?dir)
         (?level baba-objects ?objects)
         (?level baba-size ?size)
         (?level baba-rules ?rules))
   do (if (not (won? ?objects ?rules))
          (Wish ?level 'set-state (step ?objects ?size ?rules ?dir))))

  (When ((?level baba-objects ?objects)
         (?level baba-rules ?rules))
   do (if (won? ?objects ?rules) (Wish ?level 'solved #t)))
)))

; Draws every level on the table, to the right of its page, one object at a time.
(define projector (add-page (make-page-code
  (define (num n) (number->string (exact->inexact n)))

  (define (svg-element! name attrs)
    (let ((svg (query-selector (get-element-by-id "table") "svg"))
          (elem (make-svg-element name)))
      (for-each (lambda (attr) (set-attribute! elem (car attr) (cdr attr))) attrs)
      (append-child! svg elem)
      elem))

  (define (rect! x y w h fill . more)
    (svg-element! "rect" (append `(("x" . ,(num x)) ("y" . ,(num y))
                                   ("width" . ,(num w)) ("height" . ,(num h))
                                   ("fill" . ,fill))
                                 more)))

  ; cells are half a page wide, and the grid starts just right of the page
  (define (cell w) (/ w 2))
  (define (cell-x x w pos) (+ x w 10 (* (cell w) (car pos))))
  (define (cell-y y w pos) (+ y (* (cell w) (cdr pos))))

  (define (draw-thing! kind left top size)
    (let ((mx (+ left (/ size 2)))
          (my (+ top (/ size 2))))
      (case kind
        ((wall) (rect! left top size size "#555"))
        ((rock) (svg-element! "circle" `(("cx" . ,(num mx)) ("cy" . ,(num my))
                                         ("r" . ,(num (* size 0.38))) ("fill" . "sienna"))))
        ((flag)
         (rect! (+ left (* size 0.25)) (+ top (* size 0.15)) 2 (* size 0.7) "#333")
         (svg-element! "polygon"
           `(("points" . ,(string-append (num (+ left (* size 0.25))) "," (num (+ top (* size 0.15))) " "
                                         (num (+ left (* size 0.8))) "," (num (+ top (* size 0.3))) " "
                                         (num (+ left (* size 0.25))) "," (num (+ top (* size 0.45)))))
             ("fill" . "gold"))))
        ((baba) (rect! (+ left (* size 0.15)) (+ top (* size 0.2)) (* size 0.7) (* size 0.6) "orchid"
                       '("rx" . "6")))
        (else (rect! (+ left 4) (+ top 4) (- size 8) (- size 8) "lightgray")))))

  (define (word-color word)
    (case word
      ((is) "white")
      ((you) "orchid")
      ((win) "gold")
      ((stop) "lightgreen")
      ((push) "peru")
      (else "pink")))

  (define (draw-text! word left top size)
    (rect! (+ left 2) (+ top 2) (- size 4) (- size 4) "#222" '("rx" . "3"))
    (let ((label (svg-element! "text"
                   `(("x" . ,(num (+ left (/ size 2)))) ("y" . ,(num (+ top (/ size 2))))
                     ("text-anchor" . "middle") ("dominant-baseline" . "central")
                     ("fill" . ,(word-color word))
                     ("style" . ,(string-append "font: bold " (num (* size 0.27)) "px sans-serif"))))))
      (set-inner-html! label (list->string (map char-upcase (string->list (symbol->string word)))))))

  (When ((?level baba-thing (?kind ?pos))
         (?level (page left) ?x)
         (?level (page top) ?y)
         (?level (page width) ?w))
   do (draw-thing! ?kind (cell-x ?x ?w ?pos) (cell-y ?y ?w ?pos) (cell ?w)))

  (When ((?level baba-text (?word ?pos))
         (?level (page left) ?x)
         (?level (page top) ?y)
         (?level (page width) ?w))
   do (draw-text! ?word (cell-x ?x ?w ?pos) (cell-y ?y ?w ?pos) (cell ?w)))

  ; text that is part of an active rule gets a border
  (When ((?level baba-active-text ?pos)
         (?level (page left) ?x)
         (?level (page top) ?y)
         (?level (page width) ?w))
   do (rect! (cell-x ?x ?w ?pos) (cell-y ?y ?w ?pos) (cell ?w) (cell ?w) "none"
             '("stroke" . "gold") '("stroke-width" . "2")))

  (When ((?level baba-moves ?moves)
         (?level baba-size (?columns . ?rows))
         (?level (page left) ?x)
         (?level (page top) ?y)
         (?level (page width) ?w))
   do (let ((text (svg-element! "text"
                    `(("x" . ,(num (cell-x ?x ?w '(0 . 0))))
                      ("y" . ,(num (+ (cell-y ?y ?w (cons 0 ?rows)) 16)))
                      ("style" . "font: 14px sans-serif")))))
        (set-inner-html! text (format #f "moves: ~a" ?moves))))

  (When ((?level baba-solved #t))
   do (set-background! (get-page ?level) "gold"))
)))

; Controls: when a keyboard points at a level, turn its key presses into wishes.
(define wasd (add-page (make-page-code
  (define keymap
    '((#\w . up) (#\a . left) (#\s . down) (#\d . right) (#\u . undo) (#\r . reset)))
  (When ((?k keyboard #t)
         (?k points-at ?level)
         (?level baba-level #t)
         (?k key-pressed ?key))
   do (let ((binding (assv ?key keymap)))
        (if binding
            (case (cdr binding)
              ((undo) (Wish ?level 'undo #t))
              ((reset) (Wish ?level 'reset #t))
              (else (Wish ?level 'move (cdr binding)))))))
)))

(define keyboard (add-keyboard))

; whiskers, rotation-aware version from keyboard.scm. doubles as a pointing device!
(define whiskers (add-page (make-page-code
  (define pi 3.1415926536)
  (define (css-deg->radians deg)
    (let* ((adjusted (- 450 deg))
           (wrapped (modulo adjusted 360))
           (radians (* wrapped (/ pi 180))))
      radians))
  (define (get-px str)
    (let* ((substr (substring str 0 (- (string-length str) 2))))
      (string->number substr)))

  (When ((?p wishes (?p has-whiskers #t))) do
    (Claim ?p 'has-whiskers #t))

  (When ((?p has-whiskers #t)
         (?p (page left) ?x)
         (?p (page top) ?y)
         (?p (page width) ?width)
         (?p (page height) ?height)
         (?p (page rotation) ?degrees))
   do (let* ((table-div (get-element-by-id "table"))
             (svg (query-selector table-div "svg"))
             (line (make-svg-element "line"))
             (height (get-px (get-property (computed-style (get-page ?p)) "height" )))
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
         (Claim ?p 'pointer-at (cons endx endy))))

  (When ((?p pointer-at ?point)
         (?q (page left) ?qx)
         (?q (page top) ?qy)
         (?q (page width) ?qw)
         (?q (page height) ?qh))
   do (let ((px (car ?point))
            (py (cdr ?point)))
        (if (and (> px ?qx)
                 (< px (+ ?qx ?qw))
                 (> py ?qy)
                 (< py (+ ?qy ?qh)))
           (Claim ?p 'points-at ?q))))
)))

(define (add-text pagediv text)
  (let ((div (make-element "div")))
    (append-child! div (make-text-node text))
    (append-child! pagediv div)))

(define (place! pid parent text left)
  (let ((div (get-page pid)))
    (append-child! parent div)
    (add-text div text)
    (set-style-left! div left)))

(place! keyboard pages "keyboard" "15vw")
(place! whiskers pages "whiskers" "23vw")
(place! words pages "words" "31vw")
(place! grammar pages "grammar" "39vw")
(place! physics pages "physics" "47vw")
(place! projector pages "projector" "55vw")
(place! wasd pages "wasd" "63vw")
(place! wall-is-stop more-pages "wall is stop" "15vw")
(place! wall-is-win more-pages "wall is win" "23vw")
(place! rock-is-you more-pages "rock is you" "31vw")

(recalculate-pages)
