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

; Sokoban again, but written the way PuzzleScript would: a handful of small rules about
; who wants to move where, and an engine that resolves those wants into actual movement.
; Each section of a PuzzleScript file becomes one or more pages:
;   LEGEND, LEVELS     -> level pages
;   RULES              -> one page per rule (push, and pull to swap in for it)
;   COLLISIONLAYERS    -> the movement page, which is the engine's movement step
;   WINCONDITIONS      -> the win page
; plus a geometry page, since datalog cannot add 1 to a coordinate itself.
;
; A turn takes two ticks:
;   tick 1: the level claims (level wants ((player pos) dir)) for every player.
;           Rules spread wants to other things during this tick's fixpoint;
;           this is PuzzleScript's "apply every rule as often as it can".
;           The level collects every want claimed about it.
;   tick 2: the level claims all collected wants as one list, and the movement page
;           resolves them into new positions.
; Facts each level claims, every tick:
;   (level thing (kind pos))     one per object, e.g. (crate (3 . 2))
;   (level objects (thing ...))  all of them as one list
;   (level moves n)

(define pages (get-element-by-id "pages"))
(define more-pages (get-element-by-id "more-pages"))

; LEGEND and LEVELS
(define (level-page rows)
  (add-page (make-page-code
    (define legend
      '((#\# wall) (#\$ crate) (#\@ player) (#\. target)
        (#\* crate target) (#\+ player target)))

    (define (parse rows)
      (let loop-rows ((rows rows) (y 0) (acc '()))
        (if (null? rows)
            (reverse acc)
            (let loop-cols ((cs (string->list (car rows))) (x 0) (acc acc))
              (if (null? cs)
                  (loop-rows (cdr rows) (+ y 1) acc)
                  (let ((kinds (cdr (or (assv (car cs) legend) '(#f)))))
                    (loop-cols (cdr cs) (+ x 1)
                               (append (map (lambda (k) (list k (cons x y))) kinds) acc))))))))

    (define init (parse rows))
    (define objects init)
    (define history '())
    (define solved #f)
    ; bumped on every change to objects, so we know whether collected wants still apply
    (define version 0)
    ; move directions waiting for their turn, oldest first
    (define pending '())
    ; wants claimed about this level so far this tick, and the version they were derived from
    (define wants '())
    (define wants-version -1)

    (define (change! next)
      (set! objects next)
      (set! version (+ version 1)))

    (Claim this 'sokoban-level #t)

    ; runs first thing each tick, since it only depends on our own claim
    (When ((this sokoban-level #t)) do
      (let ((collected wants)
            (fresh (= wants-version version)))
        (set! wants '())
        (set! wants-version version)
        (Claim this 'objects objects)
        (for-each (lambda (obj) (Claim this 'thing obj)) objects)
        (Claim this 'moves (length history))
        (if solved (Claim this 'solved #t))
        (cond
          (solved #f)
          ; last tick's wants have spread as far as the rules take them: resolve them
          ((and fresh (pair? collected))
           (Claim this 'moving collected))
          ; start a turn: every player wants to move
          ((pair? pending)
           (let ((dir (car pending)))
             (set! pending (cdr pending))
             (for-each (lambda (obj)
                         (if (eq? (car obj) 'player)
                             (Claim this 'wants (list obj dir))))
                       objects))))))

    (When ((this wants ?want)) do
      (set! wants (cons ?want wants)))

    (When ((?p wishes (this move ?dir))) do
      (if (not solved) (set! pending (append pending (list ?dir)))))

    ; the movement page's answer; unchanged if nothing could move
    (When ((?p wishes (this set-state ?next))) do
      (if (and (not solved) (not (equal? ?next objects)))
          (begin (set! history (cons objects history))
                 (change! ?next))))

    ; The win page sends along the objects it judged. If an undo or reset got in first,
    ; those are no longer our objects, and the level must not lock in an unsolved state.
    (When ((?p wishes (this solved ?judged))) do
      (if (equal? ?judged objects) (set! solved #t)))

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

; Datalog cannot do arithmetic on positions, so this page claims which cell is next to which,
; the same trick as verbs claiming their rows in Baba Is You. Only cells around something
; that wants to move are ever needed: in front of it (for push) and behind it (for pull).
(define geometry (add-page (make-page-code
  (define (step pos dir)
    (case dir
      ((up) (cons (car pos) (- (cdr pos) 1)))
      ((down) (cons (car pos) (+ (cdr pos) 1)))
      ((left) (cons (- (car pos) 1) (cdr pos)))
      ((right) (cons (+ (car pos) 1) (cdr pos)))))

  (define (opposite dir)
    (case dir ((up) 'down) ((down) 'up) ((left) 'right) ((right) 'left)))

  (When ((?level wants ((?kind ?pos) ?dir)))
   do (Claim ?level 'next (list ?pos ?dir (step ?pos ?dir)))
      (Claim ?level 'next (list (step ?pos (opposite ?dir)) ?dir ?pos)))
)))

; [ > Player | Crate ] -> [ > Player | > Crate ]
; ?dir appears on both sides, which is what PuzzleScript's relative arrow > means.
(define push (add-page (make-page-code
  (When ((?level wants ((player ?here) ?dir))
         (?level next (?here ?dir ?there))
         (?level thing (crate ?there)))
   do (Claim ?level 'wants (list (list 'crate ?there) ?dir)))
)))

; [ < Player | Crate ] -> [ < Player | < Crate ]
; Swap this in for push and crates follow you around instead.
(define pull (add-page (make-page-code
  (When ((?level wants ((player ?here) ?dir))
         (?level next (?behind ?dir ?here))
         (?level thing (crate ?behind)))
   do (Claim ?level 'wants (list (list 'crate ?behind) ?dir)))
)))

; COLLISIONLAYERS, and the engine's movement step: things on the solid layer cannot share
; a cell. Anything whose way is clear moves, which may clear the way for whatever is behind
; it, so keep going until nothing else can move. Whatever is left over does not move.
(define movement (add-page (make-page-code
  (define solid '(wall crate player))

  (define (step pos dir)
    (case dir
      ((up) (cons (car pos) (- (cdr pos) 1)))
      ((down) (cons (car pos) (+ (cdr pos) 1)))
      ((left) (cons (- (car pos) 1) (cdr pos)))
      ((right) (cons (+ (car pos) 1) (cdr pos)))))

  (define (keep pred xs)
    (cond ((null? xs) '())
          ((pred (car xs)) (cons (car xs) (keep pred (cdr xs))))
          (else (keep pred (cdr xs)))))

  ; objects is a list of (kind pos), moving a list of ((kind pos) dir)
  (define (resolve objects moving)
    (let* ((occupied (map cadr (keep (lambda (obj) (memq (car obj) solid)) objects)))
           (clear? (lambda (want) (not (member (step (cadr (car want)) (cadr want)) occupied))))
           (go (keep clear? moving)))
      (if (null? go)
          objects
          (resolve (map (lambda (obj)
                          (let ((want (assoc obj go)))
                            (if want (list (car obj) (step (cadr obj) (cadr want))) obj)))
                        objects)
                   (keep (lambda (want) (not (member want go))) moving)))))

  (When ((?level moving ?moving)
         (?level objects ?objects))
   do (Wish ?level 'set-state (resolve ?objects ?moving)))
)))

; WINCONDITIONS: All Target on Crate
(define win (add-page (make-page-code
  (define (positions kind objects)
    (let loop ((objs objects) (acc '()))
      (cond ((null? objs) acc)
            ((eq? (car (car objs)) kind) (loop (cdr objs) (cons (cadr (car objs)) acc)))
            (else (loop (cdr objs) acc)))))

  (define (all-on? a b objects)
    (let ((bs (positions b objects)))
      (let loop ((as (positions a objects)))
        (or (null? as)
            (and (member (car as) bs) (loop (cdr as)))))))

  (When ((?level objects ?objects))
   do (if (all-on? 'target 'crate ?objects) (Wish ?level 'solved ?objects)))
)))

; Draws every level on the table, to the right of its page. Overlapping things need a
; drawing order, which datalog does not have, so this draws from the objects list instead,
; bottom layer first, the way PuzzleScript uses its collision layers.
(define projector (add-page (make-page-code
  (define layers '(target wall crate player))

  (define (num n) (number->string (exact->inexact n)))

  (define (svg-element! name attrs)
    (let ((svg (query-selector (get-element-by-id "table") "svg"))
          (elem (make-svg-element name)))
      (for-each (lambda (attr) (set-attribute! elem (car attr) (cdr attr))) attrs)
      (append-child! svg elem)
      elem))

  (define (draw! kind left top size)
    (let ((mx (+ left (/ size 2)))
          (my (+ top (/ size 2))))
      (case kind
        ((wall) (svg-element! "rect" `(("x" . ,(num left)) ("y" . ,(num top))
                                       ("width" . ,(num size)) ("height" . ,(num size))
                                       ("fill" . "#555"))))
        ((crate) (svg-element! "rect" `(("x" . ,(num (+ left 2))) ("y" . ,(num (+ top 2)))
                                        ("width" . ,(num (- size 4))) ("height" . ,(num (- size 4)))
                                        ("fill" . "peru") ("opacity" . "0.8"))))
        ((target) (svg-element! "circle" `(("cx" . ,(num mx)) ("cy" . ,(num my))
                                           ("r" . ,(num (/ size 4))) ("fill" . "crimson"))))
        ((player) (svg-element! "circle" `(("cx" . ,(num mx)) ("cy" . ,(num my))
                                           ("r" . ,(num (/ size 3))) ("fill" . "royalblue")))))))

  (When ((?level objects ?objects)
         (?level moves ?moves)
         (?level (page left) ?x)
         (?level (page top) ?y)
         (?level (page width) ?w))
   do (let* ((size (/ ?w 2))
             (left (+ ?x ?w 10))
             (rows (+ 1 (apply max (map (lambda (obj) (cdr (cadr obj))) ?objects))))
             (text (svg-element! "text" `(("x" . ,(num left))
                                          ("y" . ,(num (+ ?y (* size rows) 16)))
                                          ("style" . "font: 14px sans-serif")))))
        (for-each (lambda (layer)
                    (for-each (lambda (obj)
                                (if (eq? (car obj) layer)
                                    (draw! layer
                                           (+ left (* size (car (cadr obj))))
                                           (+ ?y (* size (cdr (cadr obj))))
                                           size)))
                              ?objects))
                  layers)
        (set-inner-html! text (format #f "moves: ~a" ?moves))))

  (When ((?level solved #t))
   do (set-background! (get-page ?level) "gold"))
)))

; Controls, unchanged from the Sokoban pages
(define wasd (add-page (make-page-code
  (define keymap
    '((#\w . up) (#\a . left) (#\s . down) (#\d . right) (#\u . undo) (#\r . reset)))
  (When ((?k keyboard #t)
         (?k points-at ?level)
         (?level sokoban-level #t)
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

(place! keyboard pages "keyboard" "12vw")
(place! whiskers pages "whiskers" "20vw")
(place! geometry pages "geometry" "28vw")
(place! push pages "push" "36vw")
(place! pull pages "pull" "44vw")
(place! movement pages "movement" "52vw")
(place! win pages "win" "60vw")
(place! projector pages "projector" "68vw")
(place! wasd pages "wasd" "76vw")
(place! first-steps more-pages "first steps" "12vw")
(place! corner more-pages "corner" "20vw")
(place! warehouse more-pages "warehouse" "28vw")

(recalculate-pages)
