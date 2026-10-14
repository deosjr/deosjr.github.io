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

; Sokoban, split across pages the same way as the Mora-Jai puzzles:
; - level pages keep their own state (the board) and claim a snapshot of it each tick
; - the rules page turns 'move' wishes into 'set-state' wishes, and decides when a level is solved
; - the projector page draws any level it can see
; - controls pages turn key presses into wishes on whatever level the keyboard points at
;
; Boards are strings in the usual Sokoban text format, rows separated by newlines:
;   # wall   @ player   $ box   . goal   + player on goal   * box on goal
; A string is a single datalog value, so we sidestep the issues with matching on nested lists.
; Note that minikanren unifies atoms with eqv?, so boards can only be bound in When conditions,
; never matched against a literal.
;
; Timing: a level claims its board once per tick, in a When that only depends on its own claim.
; A move wish updates the board locally, which becomes visible to other pages next tick.
; Keys are claimed for exactly one tick, so one key press is exactly one move.

(define pages (get-element-by-id "pages"))
(define more-pages (get-element-by-id "more-pages"))

; Every level page has the same code apart from its starting board.
; In Dynamicland you would print copies of one page with a different board on each.
(define (level-page rows)
  (add-page (make-page-code
    (define (pad row width)
      (string-append row (make-string (- width (string-length row)) #\space)))
    (define (rows->board rows)
      (let* ((width (apply max (map string-length rows)))
             (padded (map (lambda (row) (pad row width)) rows)))
        (let loop ((rest (cdr padded)) (acc (car padded)))
          (if (null? rest) acc
              (loop (cdr rest) (string-append acc "\n" (car rest)))))))

    (define init (rows->board rows))
    (define board init)
    (define history '())
    ; once solved, the level ignores all wishes until it is taken off the table,
    ; which runs this page code again from scratch
    (define solved #f)

    (define (accept! new)
      (set! history (cons board history))
      (set! board new))

    (Claim this 'sokoban-level #t)

    (When ((this sokoban-level #t)) do
      (Claim this 'sokoban-state board)
      (Claim this 'sokoban-moves (length history)))

    (When ((?p wishes (this set-state ?new))) do
      (if (not solved) (accept! ?new)))

    ; The winning move comes in as its own kind of wish rather than as a set-state plus a
    ; separate 'lock' wish: wishes made in the same tick are handled in no particular order,
    ; so a lock could otherwise land first and reject the winning move itself.
    (When ((?p wishes (this set-solved-state ?new))) do
      (if (not solved)
          (begin (accept! ?new)
                 (set! solved #t))))

    (When ((?p wishes (this undo #t))) do
      (if (and (not solved) (pair? history))
          (begin (set! board (car history))
                 (set! history (cdr history)))))

    (When ((?p wishes (this reset #t))) do
      (if (not solved)
          (begin (set! board init)
                 (set! history '())))))))

(define first-steps (level-page
  '("#######"
    "#     #"
    "# .$@ #"
    "#     #"
    "#######")))

(define corner (level-page
  '("  ####"
    "###  #"
    "#  $ #"
    "# #. ##"
    "# $.@ #"
    "#   # #"
    "#######")))

(define warehouse (level-page
  '("########"
    "#  .   #"
    "# $$#  #"
    "#  @ $ #"
    "##.#  .#"
    " #     #"
    " #######")))

; The rules of Sokoban: walk onto floor, push a single box onto floor.
; Swap this page for another to play with different rules.
(define rules (add-page (make-page-code
  (define (stride board)
    (let loop ((i 0))
      (if (char=? (string-ref board i) #\newline) (+ i 1) (loop (+ i 1)))))

  (define (find-player cells)
    (let loop ((i 0))
      (if (memv (vector-ref cells i) '(#\@ #\+)) i (loop (+ i 1)))))

  (define (offset dir width)
    (case dir
      ((up) (- width))
      ((down) width)
      ((left) -1)
      ((right) 1)
      (else 0)))

  (define (goal? c) (memv c '(#\. #\+ #\*)))
  (define (floor? c) (memv c '(#\space #\.)))
  (define (box? c) (memv c '(#\$ #\*)))

  ; returns the new board, or #f if the move is blocked
  (define (step board dir)
    (let* ((cells (list->vector (string->list board)))
           (n (vector-length cells))
           (d (offset dir (stride board)))
           (p (find-player cells))
           (t (+ p d))
           (t2 (+ t d)))
      (define (cell i) (if (and (>= i 0) (< i n)) (vector-ref cells i) #\#))
      ; put something in a cell, keeping track of whether it is a goal
      (define (put! i what)
        (let ((g (goal? (cell i))))
          (vector-set! cells i
            (case what
              ((player) (if g #\+ #\@))
              ((box) (if g #\* #\$))
              (else (if g #\. #\space))))))
      (cond
        ((floor? (cell t))
         (put! p 'empty)
         (put! t 'player)
         (list->string (vector->list cells)))
        ((and (box? (cell t)) (floor? (cell t2)))
         (put! t2 'box)
         (put! p 'empty)
         (put! t 'player)
         (list->string (vector->list cells)))
        (else #f))))

  (define (solved? board)
    (not (memv #\$ (string->list board))))

  (When ((?p wishes (?level move ?dir))
         (?level sokoban-state ?board))
   do (let ((next (step ?board ?dir)))
        (cond ((not next) #f)
              ((solved? next) (Wish ?level 'set-solved-state next))
              (else (Wish ?level 'set-state next)))))

  (When ((?level sokoban-state ?board))
   do (if (solved? ?board) (Claim ?level 'sokoban-solved #t)))
)))

; Draws every level on the table, to the right of its page.
(define projector (add-page (make-page-code
  (define (stride board)
    (let loop ((i 0))
      (if (char=? (string-ref board i) #\newline) (+ i 1) (loop (+ i 1)))))

  (define (num n) (number->string (exact->inexact n)))

  (define (svg-element! name attrs)
    (let ((svg (query-selector (get-element-by-id "table") "svg"))
          (elem (make-svg-element name)))
      (for-each (lambda (attr) (set-attribute! elem (car attr) (cdr attr))) attrs)
      (append-child! svg elem)
      elem))

  (define (square! x y size fill)
    (svg-element! "rect" `(("x" . ,(num x)) ("y" . ,(num y))
                           ("width" . ,(num size)) ("height" . ,(num size))
                           ("fill" . ,fill))))

  (define (circle! x y r fill stroke)
    (svg-element! "circle" `(("cx" . ,(num x)) ("cy" . ,(num y)) ("r" . ,(num r))
                             ("fill" . ,fill) ("stroke" . ,stroke) ("stroke-width" . "2"))))

  (define (draw-cell! c x y size)
    (let ((mx (+ x (/ size 2)))
          (my (+ y (/ size 2))))
      (case c
        ((#\#) (square! x y size "#555"))
        ((#\$) (square! (+ x 2) (+ y 2) (- size 4) "peru"))
        ((#\*) (square! (+ x 2) (+ y 2) (- size 4) "seagreen")))
      (if (memv c '(#\. #\+ #\*)) (circle! mx my (/ size 6) "none" "crimson"))
      (if (memv c '(#\@ #\+)) (circle! mx my (/ size 3) "royalblue" "none"))))

  (define (draw-board! board left top size)
    (let ((width (stride board)))
      (let loop ((i 0))
        (if (< i (string-length board))
            (begin
              (draw-cell! (string-ref board i)
                          (+ left (* size (modulo i width)))
                          (+ top (* size (quotient i width)))
                          size)
              (loop (+ i 1)))))))

  (When ((?level sokoban-state ?board)
         (?level sokoban-moves ?moves)
         (?level (page left) ?x)
         (?level (page top) ?y)
         (?level (page width) ?w))
   do (let* ((size (/ ?w 2))
             (left (+ ?x ?w 10))
             (rows (quotient (+ (string-length ?board) 1) (stride ?board)))
             (text (svg-element! "text" `(("x" . ,(num left))
                                          ("y" . ,(num (+ ?y (* size rows) 16)))
                                          ("style" . "font: 14px sans-serif")))))
        (draw-board! ?board left ?y size)
        (set-inner-html! text (format #f "moves: ~a" ?moves))))

  (When ((?level sokoban-solved #t))
   do (set-background! (get-page ?level) "gold"))
)))

; Controls: when a keyboard points at a level, turn its key presses into wishes.
; Two layouts on two pages; put either (or both) on the table.
(define (controls-page keymap)
  (add-page (make-page-code
    (When ((?k keyboard #t)
           (?k points-at ?level)
           (?level sokoban-level #t)
           (?k key-pressed ?key))
     do (let ((binding (assv ?key keymap)))
          (if binding
              (case (cdr binding)
                ((undo) (Wish ?level 'undo #t))
                ((reset) (Wish ?level 'reset #t))
                (else (Wish ?level 'move (cdr binding))))))))))

(define wasd (controls-page
  '((#\w . up) (#\a . left) (#\s . down) (#\d . right) (#\u . undo) (#\r . reset))))

(define hjkl (controls-page
  '((#\k . up) (#\h . left) (#\j . down) (#\l . right) (#\u . undo) (#\r . reset))))

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

(place! keyboard pages "keyboard" "20vw")
(place! whiskers pages "whiskers" "30vw")
(place! rules pages "rules" "40vw")
(place! projector pages "projector" "50vw")
(place! wasd pages "wasd" "60vw")
(place! hjkl pages "hjkl" "70vw")
(place! first-steps more-pages "first steps" "20vw")
(place! corner more-pages "corner" "30vw")
(place! warehouse more-pages "warehouse" "40vw")

(recalculate-pages)
