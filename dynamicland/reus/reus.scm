(use-modules (dom js)
             (datalog)
             (realtalk)
             (hoot ffi))

(define-foreign computed-style
    "window" "getComputedStyle"
    (ref null extern) -> (ref null extern))
(define-foreign get-property
    "element" "getProperty"
    (ref null extern) (ref string) -> (ref string))

(add-event-listener! (window) "update-realtalk" (procedure->external (lambda (e)
  (recalculate-pages))))

(make-dynamic)

; Reus on a projected table, a first version.
;
; A planet lies in the middle of the table, and every player has a giant. A giant's page is
; also its pointer: wherever its whisker points, the giant acts.
; - whisker on the planet's ring: the giant walks there, around the planet
; - whisker just past the ring, along the giant's own direction: its biotica menu. Hold the
;   whisker on one of them for a second to plant it in the patch the giant stands on.
; - anywhere else: the giant stays where it is.
;
; Pages:
; - the planet: twelve patches in a ring around it, each next to the patches either side.
;   It remembers what grows where; lift it off the table and the world starts over.
; - one page per giant. A giant remembers where it stood, so lifting its page takes the
;   giant off the planet and putting it back returns it there.
; - the codex: every bioticum, its yields and symbioses, and the rules that add them up.
; - whiskers, as in the other demos, and the projector.
;
; Two giants could plant in the same patch at the same moment. So giants only wish to plant;
; the planet collects those wishes during one tick and grants them at the start of the next,
; one per patch, the lowest giant first. The codex adds up yields the same way.

(define system-pages (get-element-by-id "pages"))

; ----------------------------------------------------------------------------------------------
; Geometry shared by the pages. Angles are counterclockwise from the right, with the
; y-axis flipped because screen coordinates grow downwards (as in unreal-talk's script 27).

(define pi 3.1415926536)
(define two-pi (* 2 pi))
(define patch-count 12)

(define (wrap-angle a)
  (- a (* two-pi (floor (/ a two-pi)))))

; the signed shortest turn from a to b
(define (angle-between a b)
  (- (wrap-angle (+ (- b a) pi)) pi))

(define (polar cx cy r a)
  (cons (+ cx (* r (cos a))) (- cy (* r (sin a)))))

(define (patch-index a)
  (modulo (inexact->exact (floor (/ (wrap-angle a) (/ two-pi patch-count)))) patch-count))

(define (patch-name k)
  (string->symbol (string-append "patch-" (number->string k))))

(define (get-px str)
  (string->number (substring str 0 (- (string-length str) 2))))

; a page's own size, regardless of how it is turned
(define (page-size pid)
  (get-px (get-property (computed-style (get-page pid)) "height")))

; ----------------------------------------------------------------------------------------------
; The planet

(define planet (add-page (make-page-code
  (define planted (make-vector patch-count #f))
  (define wishes '())

  ; lowest giant first
  (define (insert w ws)
    (cond ((null? ws) (list w))
          ((< (car w) (car (car ws))) (cons w ws))
          (else (cons (car ws) (insert w (cdr ws))))))
  (define (by-giant ws)
    (if (null? ws) '() (insert (car ws) (by-giant (cdr ws)))))

  (Claim this 'planet-page #t)
  (let loop ((k 0))
    (if (< k patch-count)
        (begin
          (Claim (patch-name k) 'patch-of (list this k))
          (Claim (patch-name k) 'adjacent (patch-name (modulo (+ k 1) patch-count)))
          (Claim (patch-name k) 'adjacent (patch-name (modulo (- k 1) patch-count)))
          (loop (+ k 1)))))

  (When ((this planet-page #t)
         (this (page left) ?x)
         (this (page top) ?y)
         (this (page width) ?w)
         (this (page height) ?h))
   do ; grant last tick's wishes to plant: one per patch, the lowest giant first
      (let loop ((ws (by-giant wishes)) (done '()))
        (if (pair? ws)
            (let ((k (cadr (car ws)))
                  (b (caddr (car ws))))
              (if (memv k done)
                  (loop (cdr ws) done)
                  (begin (vector-set! planted k b)
                         (loop (cdr ws) (cons k done)))))))
      (set! wishes '())
      (let ((u (page-size this)))
        (Claim this 'planet (list (+ ?x (/ ?w 2)) (+ ?y (/ ?h 2)) (* 1.2 u) (* 2.2 u)))
        (let loop ((k 0))
          (if (< k patch-count)
              (begin
                (if (vector-ref planted k)
                    (Claim (patch-name k) '(reus bioticum) (vector-ref planted k)))
                (loop (+ k 1)))))))

  (When ((?g wishes (this plant (?k ?b)))
         (?g giant ?kind))
   do (set! wishes (cons (list ?g ?k ?b) wishes)))
)))

; ----------------------------------------------------------------------------------------------
; Giants. Each page remembers its giant's place outside its page code, so the giant
; survives being lifted off the table.

(define giant-speed 1.2)  ; radians per second
(define dwell-ms 1000)    ; how long to hold the whisker on a bioticum to plant it
(define menu-spacing 34)

(define (menu-radius r1 i) (+ r1 46 (* menu-spacing i)))

(define (giant-page kind biotica start-angle)
  (let ((angle start-angle)
        (target #f)
        (last-t #f)
        (choice #f)
        (choice-since 0)
        (spent #f)
        (pointer-seen #f))
    (add-page (make-page-code
      (Wish this 'has-whiskers #t)
      (Claim this 'giant kind)
      (Claim this '(giant biotica) biotica)

      ; walk towards the target, never past it
      (When ((this giant ?kind)
             (?p planet (?cx ?cy ?r0 ?r1))
             (time now ?t))
       do (let ((dt (if last-t (min 0.25 (/ (- ?t last-t) 1000.)) 0.)))
            (set! last-t ?t)
            ; no whisker last tick: forget what was being chosen
            (if (not pointer-seen)
                (begin (set! choice #f) (set! spent #f)))
            (set! pointer-seen #f)
            (if target
                (let ((d (angle-between angle target))
                      (step (* giant-speed dt)))
                  (if (<= (abs d) step)
                      (begin (set! angle target) (set! target #f))
                      (set! angle (wrap-angle (+ angle (if (> d 0) step (- step))))))))
            (let ((p (polar ?cx ?cy (+ ?r1 16) angle)))
              (Claim this 'giant-at (list (car p) (cdr p) angle (patch-index angle))))
            (if target
                (Claim this 'giant-walk (list ?cx ?cy (+ ?r1 16) angle target)))
            (Claim this 'menu-items
                   (let loop ((bs biotica) (i 0))
                     (if (null? bs)
                         '()
                         (let ((p (polar ?cx ?cy (menu-radius ?r1 i) angle)))
                           (cons (list (car bs) (car p) (cdr p) angle)
                                 (loop (cdr bs) (+ i 1)))))))))

      ; where the whisker points decides what the giant does
      (When ((this giant ?kind)
             (this pointer-at ?tip)
             (?p planet (?cx ?cy ?r0 ?r1))
             (time now ?t))
       do (set! pointer-seen #t)
          (let* ((dx (- (car ?tip) ?cx))
                 (dy (- ?cy (cdr ?tip)))
                 (d (sqrt (+ (* dx dx) (* dy dy))))
                 ; the tip's position along and across the giant's own direction
                 (along (+ (* dx (cos angle)) (* dy (sin angle))))
                 (across (abs (- (* dy (cos angle)) (* dx (sin angle)))))
                 (n (length biotica))
                 (nearest (menu-radius ?r1 0))
                 (furthest (menu-radius ?r1 (- n 1))))
            (cond
              ((and (>= d ?r0) (<= d ?r1))
               ; giants stand in the middle of a patch
               (set! target (* (+ (patch-index (atan dy dx)) 0.5) (/ two-pi patch-count)))
               (set! choice #f)
               (set! spent #f))
              ((and (< across 22) (> along (- nearest 20)) (< along (+ furthest 20)))
               (let* ((i (max 0 (min (- n 1) (inexact->exact (round (/ (- along nearest) menu-spacing))))))
                      (b (list-ref biotica i)))
                 (if (not (eq? b choice))
                     (begin (set! choice b) (set! choice-since ?t) (set! spent #f)))
                 ; a walking giant can't plant: start counting once it stands still
                 (if target (set! choice-since ?t))
                 (let ((progress (min 1 (/ (- ?t choice-since) dwell-ms))))
                   (if (and (not spent) (>= progress 1))
                       (begin (Wish ?p 'plant (list (patch-index angle) b))
                              (set! spent b)))
                   (Claim this 'menu-choice (list i progress (if spent #t #f) (if target #t #f))))))
              (else
               (set! choice #f)
               (set! spent #f)))))
    ))))

(define forest-giant (giant-page 'forest '(blueberry ash) (* pi 0.583333333)))
(define rock-giant (giant-page 'rock '(moss-agate mossy-stone) (* pi 1.25)))
(define animal-giant (giant-page 'animal '(rabbit stoat) (* pi 1.75)))

; ----------------------------------------------------------------------------------------------
; The codex, after unreal-talk's script 35. Yields are (food gold science).

(define codex (add-page (make-page-code
  (define bonuses '())
  (define totals '())

  (define (add-yields a b) (map + a b))
  ; ((patch f g s) ...) -> ((patch . (f g s)) ...), summed per patch
  (define (sum-by-patch xs)
    (let loop ((xs xs) (acc '()))
      (if (null? xs)
          acc
          (let* ((x (car xs))
                 (prev (assq (car x) acc)))
            (loop (cdr xs)
                  (if prev
                      (begin (set-cdr! prev (add-yields (cdr prev) (cdr x))) acc)
                      (cons (cons (car x) (cdr x)) acc)))))))

  (Claim this 'codex #t)

  (Wish 'ash 'bioticum '("Ash" plant (0 0 15) (construction tree)))

  (Wish 'blueberry 'bioticum '("Blueberry" plant (10 0 0) (fruit undergrowth)))
  (Wish 'blueberry '(symbiosis kind adjacent) '(mineral 10 0 0))

  (Wish 'rabbit 'bioticum '("Rabbit" animal (15 0 0) (domesticable fur herbivore critter)))

  (Wish 'stoat 'bioticum '("Stoat" animal (0 5 0) (fur critter predator)))
  (Wish 'stoat '(symbiosis tag adjacent) '(critter 0 20 0))

  (Wish 'moss-agate 'bioticum '("Moss Agate" mineral (0 15 0) (gem)))
  (Wish 'moss-agate '(symbiosis kind adjacent) '(plant 0 5 0))

  (Wish 'mossy-stone 'bioticum '("Mossy Stone" mineral (0 0 20) (undergrowth element)))

  (When ((?someone wishes (?b bioticum (?name ?kind ?yield ?tags))))
   do (Claim ?b '(bioticum name) ?name)
      (Claim ?b '(bioticum kind) ?kind)
      (Claim ?b '(bioticum yield) ?yield)
      (for-each (lambda (tag) (Claim ?b '(bioticum tag) tag)) ?tags))

  ; at least one neighbour of a kind is enough: claims don't count duplicates
  (When ((?r adjacent ?o)
         (?o (reus bioticum) ?b)
         (?b (bioticum kind) ?kind))
   do (Claim ?r '(adjacent kind) ?kind))

  (When ((?r adjacent ?o)
         (?o (reus bioticum) ?b)
         (?b (bioticum tag) ?tag))
   do (Claim ?r '(adjacent tag) ?tag))

  (When ((?someone wishes (?b (symbiosis kind adjacent) (?kind ?food ?gold ?science)))
         (?r (reus bioticum) ?b)
         (?r (adjacent kind) ?kind))
   do (Claim ?r '(yield bonus) (list ?b ?kind ?food ?gold ?science)))

  (When ((?someone wishes (?b (symbiosis tag adjacent) (?tag ?food ?gold ?science)))
         (?r (reus bioticum) ?b)
         (?r (adjacent tag) ?tag))
   do (Claim ?r '(yield bonus) (list ?b ?tag ?food ?gold ?science)))

  ; every planted patch gets a zero bonus too, so each one shows up in the sums
  (When ((?r (reus bioticum) ?b)
         (?b (bioticum yield) ?yield))
   do (Claim ?r '(yield base) ?yield)
      (Claim ?r '(yield bonus) (list ?b 'base 0 0 0)))

  ; collect bonuses and totals during one tick, add them up at the start of the next
  (When ((?r (yield bonus) (?source ?reason ?food ?gold ?science)))
   do (set! bonuses (cons (list ?r ?food ?gold ?science) bonuses)))

  (When ((?r (yield total) ?yield))
   do (set! totals (cons ?yield totals)))

  (When ((this codex #t))
   do (let ((sums (sum-by-patch bonuses))
            (total (let loop ((ts totals) (acc '(0 0 0)))
                     (if (null? ts) acc (loop (cdr ts) (add-yields acc (car ts)))))))
        (set! bonuses '())
        (set! totals '())
        (for-each (lambda (s) (Claim (car s) '(yield bonus total) (cdr s))) sums)
        (Claim this 'planet-yield total)))

  (When ((?r (yield base) ?base)
         (?r (yield bonus total) ?bonus))
   do (Claim ?r '(yield total) (add-yields ?base ?bonus)))
)))

; ----------------------------------------------------------------------------------------------
; The projector draws the planet, its biotica and the giants.

(define projector (add-page (make-page-code
  (define (num n) (number->string (exact->inexact n)))
  (define (pt p) (string-append (num (car p)) "," (num (cdr p))))

  (define kind-colors '((plant . "#6FBF73") (mineral . "#C9A15A") (animal . "#D98C5F")))
  ; colour, page tint, letter
  (define giant-looks '((forest "#4FAF57" "#D6F0D4" "F")
                        (rock "#C99A2E" "#F4E5BC" "R")
                        (animal "#D9694B" "#F8D4C9" "A")))
  (define (kind-color kind) (let ((p (assq kind kind-colors))) (if p (cdr p) "#999")))
  (define (giant-look kind) (cdr (assq kind giant-looks)))

  (define names '())
  (define (name-of b)
    (let ((p (assq b names))) (if p (cdr p) (symbol->string b))))

  ; drawing happens in layers, so that the order rules fire in doesn't matter
  (define layers #f)
  (define (layer n) (vector-ref layers n))

  (define (svg! parent name attrs)
    (let ((elem (make-svg-element name)))
      (for-each (lambda (a) (set-attribute! elem (car a) (cdr a))) attrs)
      (append-child! parent elem)
      elem))
  (define (text! parent x y str attrs)
    (let ((elem (svg! parent "text" (append `(("x" . ,(num x)) ("y" . ,(num y))
                                              ("text-anchor" . "middle"))
                                            attrs))))
      (set-inner-html! elem str)
      elem))

  (define (wedge-path cx cy r0 r1 a0 a1)
    (string-append "M" (pt (polar cx cy r1 a0))
                   " A" (num r1) "," (num r1) " 0 0,0 " (pt (polar cx cy r1 a1))
                   " L" (pt (polar cx cy r0 a1))
                   " A" (num r0) "," (num r0) " 0 0,1 " (pt (polar cx cy r0 a0)) " Z"))
  (define (patch-angle k) (* k (/ two-pi patch-count)))

  ; text running around the planet: perpendicular to the direction a, kept upright
  (define (tangent-rotation a)
    (let loop ((d (- 90 (* a (/ 180 pi)))))
      (cond ((> d 90) (loop (- d 180)))
            ((<= d -90) (loop (+ d 180)))
            (else d))))
  (define (turned! x y a)
    (svg! (layer 4) "g" `(("transform" . ,(string-append "translate(" (num x) "," (num y) ") rotate("
                                                           (num (tangent-rotation a)) ")")))))
  (define (yields->string y)
    (string-append (number->string (car y)) " · " (number->string (cadr y)) " · " (number->string (caddr y))))

  (Claim this 'projector #t)

  (When ((this projector #t))
   do (let ((svg (query-selector (get-element-by-id "table") "svg")))
        ; light falls on the pages instead of covering them
        (set-attribute! svg "style" "mix-blend-mode: multiply")
        (set! layers (vector (svg! svg "g" '()) (svg! svg "g" '()) (svg! svg "g" '())
                             (svg! svg "g" '()) (svg! svg "g" '())))))

  (When ((?b (bioticum name) ?name))
   do (if (not (assq ?b names)) (set! names (cons (cons ?b ?name) names))))

  ; the planet, its empty patches, and the path the giants walk
  (When ((?p planet (?cx ?cy ?r0 ?r1)))
   do (svg! (layer 0) "circle" `(("cx" . ,(num ?cx)) ("cy" . ,(num ?cy)) ("r" . ,(num (+ ?r1 16)))
                                 ("fill" . "none") ("stroke" . "#C9D2CB") ("stroke-width" . "1")
                                 ("stroke-dasharray" . "3 5")))
      (svg! (layer 0) "circle" `(("cx" . ,(num ?cx)) ("cy" . ,(num ?cy)) ("r" . ,(num ?r0))
                                 ("fill" . "#E8EDE6") ("stroke" . "#B9C4BC")))
      (let loop ((k 0))
        (if (< k patch-count)
            (begin
              (svg! (layer 0) "path" `(("d" . ,(wedge-path ?cx ?cy ?r0 ?r1 (patch-angle k) (patch-angle (+ k 1))))
                                       ("fill" . "#F4F7F2") ("stroke" . "#B9C4BC")))
              (loop (+ k 1))))))

  ; planted patches
  (When ((?r patch-of (?p ?k))
         (?p planet (?cx ?cy ?r0 ?r1))
         (?r (reus bioticum) ?b)
         (?b (bioticum kind) ?kind))
   do (svg! (layer 1) "path" `(("d" . ,(wedge-path ?cx ?cy ?r0 ?r1 (patch-angle ?k) (patch-angle (+ ?k 1))))
                               ("fill" . ,(kind-color ?kind)) ("fill-opacity" . "0.8")
                               ("stroke" . "white") ("stroke-width" . "1.5"))))

  ; what grows there and what it yields
  (When ((?r patch-of (?p ?k))
         (?p planet (?cx ?cy ?r0 ?r1))
         (?r (reus bioticum) ?b)
         (?r (yield total) ?yield))
   do (let* ((a (patch-angle (+ ?k 0.5)))
             (p (polar ?cx ?cy (/ (+ ?r0 ?r1) 2) a))
             (g (svg! (layer 2) "g" `(("transform" . ,(string-append "translate(" (pt p) ") rotate("
                                                                       (num (tangent-rotation a)) ")"))))))
        (text! g 0 -2 (name-of ?b) '(("fill" . "#17201A") ("style" . "font: bold 10px sans-serif")))
        (text! g 0 11 (yields->string ?yield) '(("fill" . "#17201A") ("style" . "font: 10px monospace")))))

  ; the planet's total yield
  (When ((?c planet-yield ?yield)
         (?p planet (?cx ?cy ?r0 ?r1)))
   do (text! (layer 2) ?cx (- ?cy (* 0.58 ?r0)) "food · gold · science"
             '(("fill" . "#56625B") ("style" . "font: 10px sans-serif")))
      (text! (layer 2) ?cx (+ ?cy (* 0.72 ?r0)) (yields->string ?yield)
             '(("fill" . "#17201A") ("style" . "font: bold 13px sans-serif"))))

  ; giants, and their pages lit in the giant's colour
  (When ((?g giant ?kind)
         (?g giant-at (?x ?y ?a ?k)))
   do (let ((look (giant-look ?kind)))
        (set-background! (get-page ?g) (cadr look))
        (svg! (layer 3) "circle" `(("cx" . ,(num ?x)) ("cy" . ,(num ?y)) ("r" . "14")
                                   ("fill" . ,(car look)) ("stroke" . "#1E2620") ("stroke-width" . "1.5")))
        (text! (layer 3) ?x (+ ?y 4.5) (caddr look) '(("fill" . "#10140F") ("style" . "font: bold 13px sans-serif")))))

  ; a walking giant's way to its target
  (When ((?g giant ?kind)
         (?g giant-walk (?cx ?cy ?rad ?from ?to)))
   do (let ((d (angle-between ?from ?to))
            (end (polar ?cx ?cy ?rad ?to))
            (look (giant-look ?kind)))
        (svg! (layer 3) "path" `(("d" . ,(string-append "M" (pt (polar ?cx ?cy ?rad ?from))
                                                         " A" (num ?rad) "," (num ?rad) " 0 0,"
                                                         (if (> d 0) "0 " "1 ") (pt end)))
                                 ("fill" . "none") ("stroke" . ,(car look)) ("stroke-width" . "2.5")
                                 ("stroke-dasharray" . "6 5")))
        (svg! (layer 3) "circle" `(("cx" . ,(num (car end))) ("cy" . ,(num (cdr end))) ("r" . "5")
                                   ("fill" . ,(car look))))))

  ; each giant's menu: faint dots, until its whisker points there
  (When ((?g giant ?kind)
         (?g menu-items ?items)
         (?b (bioticum kind) ?bkind))
   do (for-each (lambda (item)
                  (if (eq? (car item) ?b)
                      (svg! (layer 3) "circle" `(("cx" . ,(num (cadr item))) ("cy" . ,(num (caddr item))) ("r" . "5")
                                                 ("fill" . ,(kind-color ?bkind)) ("fill-opacity" . "0.55")))))
                ?items))

  ; the open menu: the patch it will plant in, and a bar that fills while the whisker holds still
  (When ((?g giant ?kind)
         (?g giant-at (?x ?y ?a ?k))
         (?g menu-items ?items)
         (?g menu-choice (?i ?progress ?planted ?walking))
         (?p planet (?cx ?cy ?r0 ?r1)))
   do (let ((look (giant-look ?kind)))
        (svg! (layer 4) "path" `(("d" . ,(wedge-path ?cx ?cy ?r0 ?r1 (patch-angle ?k) (patch-angle (+ ?k 1))))
                                 ("fill" . "none") ("stroke" . ,(car look)) ("stroke-width" . "3")))
        (let loop ((items ?items) (n 0))
          (if (pair? items)
              (let* ((item (car items))
                     (b (car item))
                     (label (name-of b))
                     (w (+ 20 (* 7 (string-length label))))
                     (g (turned! (cadr item) (caddr item) (cadddr item)))
                     (chosen (= n ?i)))
                (svg! g "rect" `(("x" . ,(num (- (/ w 2)))) ("y" . "-10") ("width" . ,(num w)) ("height" . "20")
                                 ("rx" . "10") ("fill" . "white") ("stroke" . ,(car look))
                                 ("stroke-width" . ,(if chosen "2" "1"))))
                (if chosen
                    (svg! g "rect" `(("x" . ,(num (- (/ w 2)))) ("y" . "-10") ("width" . ,(num (* w ?progress)))
                                     ("height" . "20") ("rx" . "10") ("fill" . ,(car look))
                                     ("fill-opacity" . "0.7"))))
                (text! g 0 4 label '(("fill" . "#10140F") ("style" . "font: bold 11px sans-serif")))
                (loop (cdr items) (+ n 1)))))
        (let ((p (polar ?cx ?cy (+ (menu-radius ?r1 (- (length ?items) 1)) 26) ?a)))
          (text! (layer 4) (car p) (+ (cdr p) 4)
                 (cond (?walking "walking…") (?planted "planted") (else "hold to plant"))
                 '(("fill" . "#56625B") ("style" . "font: 11px sans-serif"))))))
)))

; ----------------------------------------------------------------------------------------------
; whiskers, rotation-aware version from keyboard.scm. doubles as a pointing device!

(define whiskers (add-page (make-page-code
  (define (css-deg->radians deg)
    (let* ((adjusted (- 450 deg))
           (wrapped (modulo adjusted 360))
           (radians (* wrapped (/ pi 180))))
      radians))

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

(define (add-label pagediv text)
  (let ((div (make-element "div")))
    (set-attribute! div "class" "label")
    (append-child! div (make-text-node text))
    (append-child! pagediv div)))

(define (place! pid text left)
  (let ((div (get-page pid)))
    (append-child! system-pages div)
    (add-label div text)
    (set-style-left! div left)
    div))

(place! planet "planet" "2vh")
(place! codex "codex" "12vh")
(place! forest-giant "Forest giant" "22vh")
(place! rock-giant "Rock giant" "32vh")
(place! animal-giant "Animal giant" "42vh")
(place! whiskers "whiskers" "52vh")
(place! projector "projector" "62vh")

(recalculate-pages)
