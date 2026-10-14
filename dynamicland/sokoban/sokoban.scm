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
; - level pages keep their own state (player and boxes) and claim a snapshot of it each tick
; - the rules page turns 'move' wishes into 'set-state' wishes, and decides when a level is solved
; - the projector page draws any level it can see
; - controls pages turn key presses into wishes on whatever level the keyboard points at
;
; Levels are written in the usual Sokoban text format:
;   # wall   @ player   $ box   . goal   + player on goal   * box on goal
; but a level page parses that into positions (x . y) when it first lands on the table.
; Nothing else sees the text. Each level claims:
;   (level sokoban-walls (pos ...))   static, claimed once
;   (level sokoban-goals (pos ...))   static, claimed once
;   (level sokoban-player pos)        each tick
;   (level sokoban-boxes (pos ...))   each tick
;   (level sokoban-moves n)           each tick
;
; Timing: a level claims its state once per tick, in a When that only depends on its own claim.
; A move wish updates the state locally, which becomes visible to other pages next tick.
; Keys are claimed for exactly one tick, so one key press is exactly one move.

(define pages (get-element-by-id "pages"))
(define more-pages (get-element-by-id "more-pages"))

; Every level page has the same code apart from its starting board.
; In Dynamicland you would print copies of one page with a different board on each.
(define (level-page rows)
  (add-page (make-page-code
    (define walls '())
    (define goals '())
    (define init-boxes '())
    (define init-player #f)

    (let loop-rows ((rows rows) (y 0))
      (when (pair? rows)
        (let loop-cols ((cs (string->list (car rows))) (x 0))
          (when (pair? cs)
            (let ((pos (cons x y))
                  (c (car cs)))
              (if (char=? c #\#) (set! walls (cons pos walls)))
              (if (memv c '(#\. #\+ #\*)) (set! goals (cons pos goals)))
              (if (memv c '(#\$ #\*)) (set! init-boxes (cons pos init-boxes)))
              (if (memv c '(#\@ #\+)) (set! init-player pos)))
            (loop-cols (cdr cs) (+ x 1))))
        (loop-rows (cdr rows) (+ y 1))))

    (define player init-player)
    (define boxes init-boxes)
    ; previous (player boxes) states, most recent first
    (define history '())
    ; once solved, the level ignores all wishes until it is taken off the table,
    ; which runs this page code again from scratch
    (define solved #f)

    ; next is a (player boxes) list
    (define (accept! next)
      (set! history (cons (list player boxes) history))
      (set! player (car next))
      (set! boxes (cadr next)))

    (Claim this 'sokoban-level #t)
    (Claim this 'sokoban-walls walls)
    (Claim this 'sokoban-goals goals)

    (When ((this sokoban-level #t)) do
      (Claim this 'sokoban-player player)
      (Claim this 'sokoban-boxes boxes)
      (Claim this 'sokoban-moves (length history)))

    (When ((?p wishes (this set-state ?next))) do
      (if (not solved) (accept! ?next)))

    ; The winning move comes in as its own kind of wish rather than as a set-state plus a
    ; separate 'lock' wish: wishes made in the same tick are handled in no particular order,
    ; so a lock could otherwise land first and reject the winning move itself.
    (When ((?p wishes (this set-solved-state ?next))) do
      (if (not solved)
          (begin (accept! ?next)
                 (set! solved #t))))

    (When ((?p wishes (this undo #t))) do
      (if (and (not solved) (pair? history))
          (begin (set! player (car (car history)))
                 (set! boxes (cadr (car history)))
                 (set! history (cdr history)))))

    (When ((?p wishes (this reset #t))) do
      (if (not solved)
          (begin (set! player init-player)
                 (set! boxes init-boxes)
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
  (define (offset dir)
    (case dir
      ((up) '(0 . -1))
      ((down) '(0 . 1))
      ((left) '(-1 . 0))
      ((right) '(1 . 0))
      (else '(0 . 0))))

  (define (add pos d)
    (cons (+ (car pos) (car d)) (+ (cdr pos) (cdr d))))

  ; returns the next (player boxes), or #f if the move is blocked
  (define (step walls boxes player dir)
    (let* ((d (offset dir))
           (target (add player d))
           (beyond (add target d)))
      (cond
        ((member target walls) #f)
        ((not (member target boxes)) (list target boxes))
        ((or (member beyond walls) (member beyond boxes)) #f)
        (else (list target (map (lambda (b) (if (equal? b target) beyond b)) boxes))))))

  (define (solved? goals boxes)
    (let loop ((bs boxes))
      (cond ((null? bs) #t)
            ((member (car bs) goals) (loop (cdr bs)))
            (else #f))))

  (When ((?p wishes (?level move ?dir))
         (?level sokoban-walls ?walls)
         (?level sokoban-goals ?goals)
         (?level sokoban-boxes ?boxes)
         (?level sokoban-player ?player))
   do (let ((next (step ?walls ?boxes ?player ?dir)))
        (cond ((not next) #f)
              ((solved? ?goals (cadr next)) (Wish ?level 'set-solved-state next))
              (else (Wish ?level 'set-state next)))))

  (When ((?level sokoban-goals ?goals)
         (?level sokoban-boxes ?boxes))
   do (if (solved? ?goals ?boxes) (Claim ?level 'sokoban-solved #t)))
)))

; Draws every level on the table, to the right of its page.
(define projector (add-page (make-page-code
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

  (When ((?level sokoban-walls ?walls)
         (?level sokoban-goals ?goals)
         (?level sokoban-boxes ?boxes)
         (?level sokoban-player ?player)
         (?level sokoban-moves ?moves)
         (?level (page left) ?x)
         (?level (page top) ?y)
         (?level (page width) ?w))
   do (let* ((size (/ ?w 2))
             (left (+ ?x ?w 10))
             (rows (+ 1 (apply max (map cdr ?walls))))
             ; top-left corner and center of a grid position
             (x0 (lambda (pos) (+ left (* size (car pos)))))
             (y0 (lambda (pos) (+ ?y (* size (cdr pos)))))
             (cx (lambda (pos) (+ (x0 pos) (/ size 2))))
             (cy (lambda (pos) (+ (y0 pos) (/ size 2))))
             (text (svg-element! "text" `(("x" . ,(num left))
                                          ("y" . ,(num (+ ?y (* size rows) 16)))
                                          ("style" . "font: 14px sans-serif")))))
        ; drawn in this order so goal markers show on top of boxes, and the player on top of goals
        (for-each (lambda (pos) (square! (x0 pos) (y0 pos) size "#555")) ?walls)
        (for-each (lambda (pos)
                    (square! (+ (x0 pos) 2) (+ (y0 pos) 2) (- size 4)
                             (if (member pos ?goals) "seagreen" "peru")))
                  ?boxes)
        (for-each (lambda (pos) (circle! (cx pos) (cy pos) (/ size 6) "none" "crimson")) ?goals)
        (circle! (cx ?player) (cy ?player) (/ size 3) "royalblue" "none")
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
