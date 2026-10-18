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

; Netrunner on a projected table, a first version.
; See https://rules.nullsignal.games/ for the rules this follows, scoped down a lot.
;
; Pages:
; - identity cards for both players. The Corp's servers are projected relative to the Corp
;   identity, the Runner's rig zone relative to the Runner identity: move one and its layout follows.
; - card pages. Each card's page code claims its own card text.
; - pointers with whiskers: the Corp's installer and wand (rotated to point down at the board),
;   the Runner's wand and run token (pointing up).
; - the referee keeps the books: turns, clicks, credits, which server slots exist, the run.
; - the projector draws whatever the referee asks it to.
;
; Card backs carry no code. The table never needs to know what a face-down card is, only that
; a slot exists: the installer adds a slot, and the Corp lays the card face down in it. A card's
; identity only matters face up: when it is rezzed in its slot, or when it lies in the resolution
; box (played, scored or accessed). In the browser, press f over a card to flip it: a face-down
; card can't be read, so the table doesn't see it at all.
;
; Like the Baba level pages, the referee collects what it sees during one tick and acts on it at
; the start of the next. Only pages that have stopped moving count, so dragging a card or a wand
; across the table does nothing until it is put down.

(define system-pages (get-element-by-id "pages"))
(define corp-shelf (get-element-by-id "corp-cards"))
(define runner-shelf (get-element-by-id "runner-cards"))

; ----------------------------------------------------------------------------------------------
; Cards. Each card page claims (this card (name data)), where data is its card text.

(define (card-page name data)
  (add-page (make-page-code
    (Claim this 'card (list name data)))))

(define hedge-fund
  '((label "Hedge Fund") (side corp) (type operation) (cost 5) (gain 9)))
(define ice-wall
  '((label "Ice Wall") (side corp) (type ice) (subtype barrier) (rez 1) (strength 1)
    (subs (end-the-run))))
(define enigma
  '((label "Enigma") (side corp) (type ice) (subtype code-gate) (rez 3) (strength 2)
    (subs (lose-click end-the-run))))
(define pad-campaign
  '((label "PAD Campaign") (side corp) (type asset) (rez 2) (trash 4) (turn-gain 1)))
; agenda text is ignored in this version
(define priority-requisition
  '((label "Priority Requisition") (side corp) (type agenda) (advance 5) (points 3)))
(define hostile-takeover
  '((label "Hostile Takeover") (side corp) (type agenda) (advance 2) (points 1)))
(define private-security-force
  '((label "Private Security Force") (side corp) (type agenda) (advance 4) (points 2)))

(define sure-gamble
  '((label "Sure Gamble") (side runner) (type event) (cost 5) (gain 9)))
(define easy-mark
  '((label "Easy Mark") (side runner) (type event) (cost 0) (gain 3)))
(define corroder
  '((label "Corroder") (side runner) (type program) (install 2) (mu 1) (strength 2)
    (breaks barrier) (break-cost 1) (boost-cost 1) (boost-lasts encounter)))
(define gordian-blade
  '((label "Gordian Blade") (side runner) (type program) (install 4) (mu 1) (strength 2)
    (breaks code-gate) (break-cost 1) (boost-cost 1) (boost-lasts run)))
(define armitage-codebusting
  '((label "Armitage Codebusting") (side runner) (type resource) (install 1) (holds 12) (take 2)))

(define corp-deck
  (list (cons 'hedge-fund hedge-fund) (cons 'hedge-fund hedge-fund)
        (cons 'ice-wall ice-wall) (cons 'ice-wall ice-wall)
        (cons 'enigma enigma) (cons 'enigma enigma)
        (cons 'pad-campaign pad-campaign)
        (cons 'priority-requisition priority-requisition)
        (cons 'hostile-takeover hostile-takeover) (cons 'hostile-takeover hostile-takeover)
        (cons 'private-security-force private-security-force)))

(define runner-deck
  (list (cons 'sure-gamble sure-gamble) (cons 'sure-gamble sure-gamble)
        (cons 'easy-mark easy-mark)
        (cons 'corroder corroder) (cons 'gordian-blade gordian-blade)
        (cons 'armitage-codebusting armitage-codebusting)))

; ----------------------------------------------------------------------------------------------
; Identities and pointers

(define corp-identity (add-page (make-page-code
  (Claim this 'identity 'corp))))

(define runner-identity (add-page (make-page-code
  (Claim this 'identity 'runner))))

(define (pointer-page role)
  (add-page (make-page-code
    (Wish this 'has-whiskers #t)
    (Claim this 'pointer-role role))))

(define installer (pointer-page 'installer))
(define corp-wand (pointer-page 'corp-wand))
(define runner-wand (pointer-page 'runner-wand))
(define run-token (pointer-page 'run-token))

; ----------------------------------------------------------------------------------------------
; The referee

(define referee (add-page (make-page-code
  ; ---- small helpers
  (define (get key data . default)
    (let ((p (assq key data)))
      (if p (cadr p) (if (null? default) #f (car default)))))
  (define (keep pred xs)
    (cond ((null? xs) '())
          ((pred (car xs)) (cons (car xs) (keep pred (cdr xs))))
          (else (keep pred (cdr xs)))))
  (define (find pred xs)
    (cond ((null? xs) #f) ((pred (car xs)) (car xs)) (else (find pred (cdr xs)))))
  (define (index-of x xs)
    (let loop ((xs xs) (i 0))
      (cond ((null? xs) #f) ((eq? (car xs) x) i) (else (loop (cdr xs) (+ i 1))))))
  (define (n->s n) (number->string n))
  (define (inside? px py r)
    (and (>= px (list-ref r 0)) (<= px (+ (list-ref r 0) (list-ref r 2)))
         (>= py (list-ref r 1)) (<= py (+ (list-ref r 1) (list-ref r 3)))))
  (define (alist-ref key alist) (let ((p (assoc key alist))) (and p (cdr p))))
  (define (alist-set key value alist)
    (cons (cons key value) (keep (lambda (p) (not (equal? (car p) key))) alist)))
  (define (alist-remove key alist)
    (keep (lambda (p) (not (equal? (car p) key))) alist))

  ; ---- game state
  (define turn 'corp)
  (define turn-number 1)
  (define clicks 3)
  (define corp-credits 5)
  (define runner-credits 5)
  (define corp-score 0)
  (define runner-score 0)
  ; nominal 30-card decks, counted for the status display and the R&D-empty win
  (define rnd-count 24)
  (define hq-count 6)
  (define stack-count 25)
  (define grip-count 5)
  (define winner #f)

  ; a slot is #(face card counters): face is down or up, card is (name . data) once rezzed
  (define (new-slot) (vector 'down #f 0))
  (define (slot-face s) (vector-ref s 0))
  (define (slot-card s) (vector-ref s 1))
  (define (slot-counters s) (vector-ref s 2))
  ; a server is #(name kind ice root): ice is a list of slots, innermost first;
  ; root is a slot for remote servers, #f otherwise
  (define (server-name s) (vector-ref s 0))
  (define (server-kind s) (vector-ref s 1))
  (define (server-ice s) (vector-ref s 2))
  (define (server-root s) (vector-ref s 3))
  (define servers
    (list (vector "HQ" 'hq '() #f) (vector "R&D" 'rnd '() #f) (vector "Archives" 'archives '() #f)))
  (define remotes-made 0)
  (define max-ice 2)

  ; installed Runner cards: #(page name data credits-on-card)
  (define rig '())

  ; the run, or #f: #(server position phase broken encounter-boosts run-boosts)
  ; position counts ice from the innermost (1); 0 means the server itself
  (define run #f)
  (define (run-server) (vector-ref run 0))
  (define (run-pos) (vector-ref run 1))
  (define (run-phase) (vector-ref run 2))
  (define (set-phase! p) (vector-set! run 2 p))

  (define selected #f)          ; (server . slot-key) chosen with the Corp's wand
  (define remove-candidate #f)  ; (server . slot-key) the installer points at
  (define pending-score #f)     ; agenda waiting for the Corp to point at its slot
  (define box-card #f)          ; card page lying in the resolution box
  (define box-note #f)          ; what to do with it next
  (define flash-text #f)
  (define flash-until 0)
  (define (flash! msg)
    (set! flash-text msg)
    (set! flash-until (+ (date-now) 4000)))

  ; ---- what was seen on the table during the previous tick
  (define cards-seen '())
  (define pointers-seen '())
  (define ids-seen '())
  ; memory between ticks
  (define last-positions '())   ; page -> (x . y)
  (define regions '())          ; page -> region it was last put down in
  (define rests '())            ; pointer page -> where its whisker last came to rest
  (define ui-targets '())       ; (id rect owner) drawn last tick
  (define ui-columns '())       ; (server rect) for the run token

  ; ---- money and clicks
  (define (credits-of side) (if (eq? side 'corp) corp-credits runner-credits))
  (define (pay! side n)
    (if (eq? side 'corp)
        (set! corp-credits (- corp-credits n))
        (set! runner-credits (- runner-credits n))))
  (define (gain! side n) (pay! side (- n)))
  (define (side-name side) (if (eq? side 'corp) "Corp" "Runner"))

  ; spend a click and some credits on an action, or explain why not
  (define (action! side cost what)
    (cond (winner #f)
          ((not (eq? turn side)) (flash! (string-append "It's the " (side-name turn) "'s turn")) #f)
          (run (flash! "Finish the run first") #f)
          ((< clicks 1) (flash! "No clicks left") #f)
          ((< (credits-of side) cost)
           (flash! (string-append what " costs " (n->s cost) "¢")) #f)
          (else (set! clicks (- clicks 1)) (pay! side cost) #t)))

  (define (check-win!)
    (cond ((>= corp-score 7) (set! winner 'corp))
          ((>= runner-score 7) (set! winner 'runner))))

  (define (start-corp-turn!)
    (set! turn 'corp)
    (set! clicks 3)
    (set! turn-number (+ turn-number 1))
    (set! selected #f)
    ; PAD Campaign and the like: gain credits when the turn begins
    (for-each (lambda (s)
                (let ((root (server-root s)))
                  (if (and root (eq? (slot-face root) 'up))
                      (gain! 'corp (get 'turn-gain (cdr (slot-card root)) 0)))))
              servers)
    ; the mandatory draw
    (if (= rnd-count 0)
        (set! winner 'runner)
        (begin (set! rnd-count (- rnd-count 1))
               (set! hq-count (+ hq-count 1))
               (flash! "Corp: draw your card for the turn"))))

  (define (start-runner-turn!)
    (set! turn 'runner)
    (set! clicks 4)
    (set! selected #f)
    (set! remove-candidate #f))

  (define (end-turn-if-done!)
    (if (and (not winner) (not run) (= clicks 0) (not pending-score))
        (if (eq? turn 'corp) (start-runner-turn!) (start-corp-turn!))))

  ; ---- slots
  (define (slot-at i key)
    (let ((s (list-ref servers i)))
      (if (eq? key 'root)
          (server-root s)
          (let ((ice (server-ice s)))
            (and (<= key (length ice)) (list-ref ice (- key 1)))))))

  (define (remove-slot! i key)
    (let ((s (list-ref servers i)))
      (if (eq? key 'root)
          (vector-set! s 3 #f)
          (let ((victim (slot-at i key)))
            (vector-set! s 2 (keep (lambda (x) (not (eq? x victim))) (server-ice s)))))))

  ; ---- the run
  (define (approaching? i k)
    (and run (eq? (run-phase) 'approach) (= (run-server) i) (equal? (run-pos) k)))

  (define (current-ice-slot) (slot-at (run-server) (run-pos)))

  (define (end-run! msg)
    (set! run #f)
    (if msg (flash! msg)))

  (define (start-run! i)
    (cond ((not (eq? turn 'runner)) (flash! "Runs happen on the Runner's turn"))
          ((action! 'runner 0 "Running")
           (set! run (vector i (length (server-ice (list-ref servers i))) 'approach '() '() '()))
           (enter-position!))))

  (define (enter-position!)
    (if (= (run-pos) 0) (begin-access!) (approach!)))

  (define (approach!)
    (set-phase! 'approach)
    (if (eq? (slot-face (current-ice-slot)) 'up) (begin-encounter!)))

  (define (begin-encounter!)
    (set-phase! 'encounter)
    (vector-set! run 3 '())
    (vector-set! run 4 '()))

  (define (pass-ice!)
    (set-phase! 'passed))

  (define (begin-access!)
    (set-phase! 'access))

  ; how to break a subroutine on this ice: (page label cost boost lasts), or #f
  (define (break-option ice-data)
    (let ((subtype (get 'subtype ice-data))
          (strength (get 'strength ice-data 0)))
      (let loop ((cards rig) (best #f))
        (if (null? cards)
            best
            (let* ((r (car cards))
                   (data (vector-ref r 2))
                   (page (vector-ref r 0)))
              (if (eq? (get 'breaks data) subtype)
                  (let* ((boosted (+ (get 'strength data 0)
                                     (or (alist-ref page (vector-ref run 4)) 0)
                                     (or (alist-ref page (vector-ref run 5)) 0)))
                         (need (max 0 (- strength boosted)))
                         (cost (+ (* need (get 'boost-cost data 1)) (get 'break-cost data 1))))
                    (loop (cdr cards)
                          (if (or (not best) (< cost (list-ref best 2)))
                              (list page (get 'label data) cost need (get 'boost-lasts data))
                              best)))
                  (loop (cdr cards) best)))))))

  (define (sub-text sub)
    (case sub
      ((end-the-run) "End the run")
      ((lose-click) "Runner loses a click")
      (else (symbol->string sub))))

  (define (break! n)
    (let* ((ice-data (cdr (slot-card (current-ice-slot))))
           (option (break-option ice-data)))
      (cond ((not option) (flash! "Nothing installed breaks this ice"))
            ((< runner-credits (list-ref option 2))
             (flash! (string-append "Breaking costs " (n->s (list-ref option 2)) "¢")))
            (else
             (pay! 'runner (list-ref option 2))
             (let ((page (list-ref option 0))
                   (need (list-ref option 3)))
               (if (> need 0)
                   (if (eq? (list-ref option 4) 'run)
                       (vector-set! run 5 (alist-set page (+ need (or (alist-ref page (vector-ref run 5)) 0))
                                                     (vector-ref run 5)))
                       (vector-set! run 4 (alist-set page (+ need (or (alist-ref page (vector-ref run 4)) 0))
                                                     (vector-ref run 4))))))
             (vector-set! run 3 (cons n (vector-ref run 3)))))))

  ; resolve unbroken subroutines in order
  (define (fire!)
    (let* ((card (slot-card (current-ice-slot)))
           (subs (get 'subs (cdr card) '())))
      (let loop ((subs subs) (n 0))
        (cond ((not run) #f)
              ((null? subs) (pass-ice!))
              ((memv n (vector-ref run 3)) (loop (cdr subs) (+ n 1)))
              (else
               (case (car subs)
                 ((end-the-run) (end-run! (string-append (get 'label (cdr card)) " ended the run")))
                 ((lose-click) (set! clicks (max 0 (- clicks 1)))))
               (loop (cdr subs) (+ n 1)))))))

  ; ---- the resolution box
  (define (access! name data)
    (let* ((i (run-server))
           (s (list-ref servers i))
           (type (get 'type data)))
      (cond
        ((eq? type 'agenda)
         (set! runner-score (+ runner-score (get 'points data 0)))
         (case (server-kind s)
           ((remote) (vector-set! s 3 #f))
           ((hq) (set! hq-count (max 0 (- hq-count 1))))
           ((rnd) (set! rnd-count (max 0 (- rnd-count 1)))))
         (set! box-note "Stolen: move it to your score area")
         (set-phase! 'leaving)
         (check-win!))
        ((get 'trash data)
         (set-phase! 'trash-choice))
        (else
         (set! box-note "Put it back where it came from")
         (set-phase! 'leaving)))))

  (define (scoring-candidates requirement)
    (let loop ((ss servers) (i 0) (acc '()))
      (if (null? ss)
          (reverse acc)
          (let ((root (server-root (car ss))))
            (loop (cdr ss) (+ i 1)
                  (if (and root (eq? (slot-face root) 'down) (>= (slot-counters root) requirement))
                      (cons i acc)
                      acc))))))

  (define (finish-score! i data)
    (vector-set! (list-ref servers i) 3 #f)
    (set! corp-score (+ corp-score (get 'points data 0)))
    (set! pending-score #f)
    (set! box-note "Scored: move it to your score area")
    (check-win!))

  (define (score! name data)
    (cond ((not (eq? turn 'corp)) (set! box-note "Agendas are scored on the Corp's turn"))
          (else
           (let ((candidates (scoring-candidates (get 'advance data 0))))
             (cond ((null? candidates)
                    (set! box-note (string-append "No installed card has "
                                                  (n->s (get 'advance data 0))
                                                  " advancement counters")))
                   ((null? (cdr candidates)) (finish-score! (car candidates) data))
                   (else
                    (set! pending-score data)
                    (set! box-note "Point your wand at the slot it came from")))))))

  (define (play! name data)
    (let ((side (get 'side data)))
      (if (action! side (get 'cost data 0) (string-append "Playing " (get 'label data)))
          (begin
            (gain! side (get 'gain data 0))
            (if (eq? side 'corp)
                (set! hq-count (max 0 (- hq-count 1)))
                (set! grip-count (max 0 (- grip-count 1))))
            (set! box-note (if (eq? side 'corp) "Done: move it to Archives" "Done: move it to the heap")))
          (set! box-note "Take it back"))))

  (define (on-box! page name data)
    (set! box-card page)
    (set! box-note #f)
    (let ((type (get 'type data)))
      (cond (winner #f)
            ((and run (eq? (run-phase) 'access)) (access! name data))
            (run (set! box-note "Nothing to resolve during a run"))
            ((memq type '(operation event)) (play! name data))
            ((eq? type 'agenda) (score! name data))
            (else (set! box-note "Install this card instead")))))

  ; ---- a card put down in a server slot: rezzing
  (define (on-slot! name data i key)
    (let ((slot (slot-at i key))
          (type (get 'type data))
          (label (get 'label data)))
      (cond
        ((not slot) #f)
        ((eq? (slot-face slot) 'up) #f)
        ((not (eq? (get 'side data) 'corp)) (flash! "Only Corp cards go in servers"))
        ((eq? type 'agenda) (flash! "Agendas aren't rezzed: score them in the resolution box"))
        ((and (number? key) (not (eq? type 'ice))) (flash! "That slot holds ice"))
        ((and (eq? key 'root) (eq? type 'ice)) (flash! "Ice goes in front of a server"))
        ((not (memq type '(ice asset))) (flash! (string-append label " can't be rezzed")))
        ((and (eq? type 'ice) (not (approaching? i key)))
         (flash! "Ice can only be rezzed while the Runner approaches it"))
        ((< corp-credits (get 'rez data 0))
         (flash! (string-append "Rezzing " label " costs " (n->s (get 'rez data 0)) "¢")))
        (else
         (pay! 'corp (get 'rez data 0))
         (vector-set! slot 0 'up)
         (vector-set! slot 1 (cons name data))
         (if (approaching? i key) (begin-encounter!))))))

  ; ---- a card put down in the rig: installing
  (define (mu-used)
    (apply + (map (lambda (r) (get 'mu (vector-ref r 2) 0)) rig)))

  (define (on-rig! page name data)
    (cond ((find (lambda (r) (equal? (vector-ref r 0) page)) rig) #f)
          ((not (eq? (get 'side data) 'runner)) (flash! "Only Runner cards go in the rig"))
          ((eq? (get 'type data) 'event) (flash! "Events are played in the resolution box"))
          ((> (+ (mu-used) (get 'mu data 0)) 4) (flash! "Not enough memory for that program"))
          ((action! 'runner (get 'install data 0) (string-append "Installing " (get 'label data)))
           (set! grip-count (max 0 (- grip-count 1)))
           (set! rig (cons (vector page name data (get 'holds data 0)) rig)))))

  (define (on-departure! page region)
    (cond ((eq? region 'rig)
           (set! rig (keep (lambda (r) (not (equal? (vector-ref r 0) page))) rig)))
          ((and (eq? region 'box) (equal? page box-card))
           (set! box-card #f)
           (set! box-note #f)
           (if (and run (eq? (run-phase) 'leaving)) (end-run! #f)))))

  (define (on-arrival! page name data region)
    (cond ((eq? region 'rig) (on-rig! page name data))
          ((eq? region 'box) (on-box! page name data))
          ((and (pair? region) (eq? (car region) 'slot))
           (on-slot! name data (cadr region) (caddr region)))))

  ; ---- a wand or installer put down on a projected target
  (define (press! id)
    (case (car id)
      ((gain)
       (let ((side (cadr id)))
         (if (action! side 0 "Gaining a credit") (gain! side 1))))
      ((draw)
       (let ((side (cadr id)))
         (cond ((and (eq? side 'runner) (= stack-count 0)) (flash! "Your stack is empty"))
               ((action! side 0 "Drawing")
                (if (eq? side 'corp)
                    (if (= rnd-count 0)
                        (set! winner 'runner)
                        (begin (set! rnd-count (- rnd-count 1)) (set! hq-count (+ hq-count 1))))
                    (begin (set! stack-count (- stack-count 1)) (set! grip-count (+ grip-count 1))))))))
      ((add-ice)
       (let* ((i (cadr id))
              (s (list-ref servers i))
              (n (length (server-ice s))))
         (cond ((>= n max-ice) (flash! "This version has room for 2 ice per server"))
               ((action! 'corp n "Installing ice here")
                (set! hq-count (max 0 (- hq-count 1)))
                (vector-set! s 2 (append (server-ice s) (list (new-slot))))))))
      ((new-remote)
       (if (action! 'corp 0 "Installing")
           (begin
             (set! remotes-made (+ remotes-made 1))
             (set! hq-count (max 0 (- hq-count 1)))
             (set! servers (append servers
                                   (list (vector (string-append "Remote " (n->s remotes-made))
                                                 'remote '() (new-slot))))))))
      ((install-root)
       (if (action! 'corp 0 "Installing")
           (begin (set! hq-count (max 0 (- hq-count 1)))
                  (vector-set! (list-ref servers (cadr id)) 3 (new-slot)))))
      ((slot)
       (set! remove-candidate (cons (cadr id) (caddr id))))
      ((confirm-remove)
       (cond (run (flash! "Wait until the run is over"))
             (remove-candidate
              (remove-slot! (car remove-candidate) (cdr remove-candidate))
              (set! remove-candidate #f)
              (flash! "Take the card off the table"))))
      ((select)
       (let ((i (cadr id)))
         (if (and pending-score (memv i (scoring-candidates (get 'advance pending-score 0))))
             (finish-score! i pending-score)
             (set! selected (cons i 'root)))))
      ((advance)
       (let ((slot (and selected (slot-at (car selected) (cdr selected)))))
         (if (and slot (action! 'corp 1 "Advancing"))
             (vector-set! slot 2 (+ (slot-counters slot) 1)))))
      ((pass-rez)
       (if (and run (eq? (run-phase) 'approach))
           (pass-ice!)))
      ((break) (if (and run (eq? (run-phase) 'encounter)) (break! (cadr id))))
      ((fire) (if (and run (eq? (run-phase) 'encounter)) (fire!)))
      ((continue)
       (if (and run (eq? (run-phase) 'passed))
           (begin (vector-set! run 1 (- (run-pos) 1)) (enter-position!))))
      ((jack-out)
       (if (and run (eq? (run-phase) 'passed)) (end-run! "You jacked out")))
      ((nothing-to-access)
       (if (and run (eq? (run-phase) 'access)) (end-run! #f)))
      ((trash-access)
       (if (and run (eq? (run-phase) 'trash-choice))
           (let ((cost (cadr id))
                 (s (list-ref servers (run-server))))
             (if (< runner-credits cost)
                 (flash! (string-append "Trashing it costs " (n->s cost) "¢"))
                 (begin (pay! 'runner cost)
                        (case (server-kind s)
                          ((remote) (vector-set! s 3 #f))
                          ((hq) (set! hq-count (max 0 (- hq-count 1))))
                          ((rnd) (set! rnd-count (max 0 (- rnd-count 1)))))
                        (set! box-note "Trashed: move it to Archives")
                        (set-phase! 'leaving))))))
      ((leave-access)
       (if (and run (eq? (run-phase) 'trash-choice))
           (begin (set! box-note "Put it back where it came from")
                  (set-phase! 'leaving))))
      ((take)
       (let ((r (find (lambda (r) (equal? (vector-ref r 0) (cadr id))) rig)))
         (if (and r (> (vector-ref r 3) 0) (action! 'runner 0 "Taking credits"))
             (let ((n (min (get 'take (vector-ref r 2) 0) (vector-ref r 3))))
               (vector-set! r 3 (- (vector-ref r 3) n))
               (gain! 'runner n)
               (if (= (vector-ref r 3) 0)
                   (flash! (string-append (get 'label (vector-ref r 2)) " is empty: move it to the heap")))))))))

  ; ---- layout, relative to the identity cards
  (define (layout-of side)
    (find (lambda (i) (eq? (car i) side)) ids-frame))
  (define ids-frame '())

  ; the table's size, for the shared resolution box and the score areas
  (define (table-size)
    (let ((r (get-bounding-client-rect (get-element-by-id "table"))))
      (cons (get-width r) (get-height r))))

  (define (classify cx cy L)
    (let ((slot (find (lambda (e) (inside? cx cy (cadr e))) (list-ref L 0))))
      (cond (slot (car slot))
            ((inside? cx cy (list-ref L 1)) 'rig)
            ((inside? cx cy (list-ref L 2)) 'box)
            ((find (lambda (r) (inside? cx cy r)) (list-ref L 3)) 'ignore)
            (else 'loose))))

  ; L = (slot-rects rig-rect box-rect ignore-rects geometry)
  (define (make-layout corp runner size)
    (let* ((cx (list-ref corp 1)) (cy (list-ref corp 2)) (u (list-ref corp 3))
           (rx (list-ref runner 1)) (ry (list-ref runner 2)) (ru (list-ref runner 3))
           (tw (car size)) (th (cdr size))
           (colw (+ u 30))
           (x0 (+ cx (* 1.7 u)))
           (y-root (+ cy (* 1.8 u)))
           (slot-size (+ u 8))
           (col-x (lambda (i) (+ x0 (* i colw))))
           (root-rect (lambda (i) (list (col-x i) y-root slot-size slot-size)))
           (ice-rect (lambda (i k) (list (col-x i) (+ y-root (* (+ u 16) k)) slot-size slot-size)))
           (slots
            (let loop ((ss servers) (i 0) (acc '()))
              (if (null? ss)
                  acc
                  (let* ((s (car ss))
                         (ice-slots (let iloop ((k 1) (acc2 '()))
                                      (if (> k (length (server-ice s)))
                                          acc2
                                          (iloop (+ k 1) (cons (list (list 'slot i k) (ice-rect i k)) acc2)))))
                         (root (if (server-root s) (list (list (list 'slot i 'root) (root-rect i))) '())))
                    (loop (cdr ss) (+ i 1) (append root ice-slots acc))))))
           (rig-rect (list (+ rx ru 190) (- ry 12) (* 6.2 ru) (+ ru 24)))
           (heap-rect (list (+ rx ru 210 (* 6.2 ru)) (- ry 6) (+ ru 12) (+ ru 12)))
           (box-rect (list (- tw (* 2.4 u)) (- (/ th 2) (* 1.1 u)) (* 1.9 u) (* 1.9 u)))
           (corp-score-rect (list (- tw (* 2.1 u)) 10 (* 1.8 u) (* 1.8 u)))
           (runner-score-rect (list (- tw (* 2.1 u)) (- th (* 1.9 u)) (* 1.8 u) (* 1.8 u)))
           (piles (list (root-rect 0) (root-rect 1) (root-rect 2))))
      (list slots rig-rect box-rect
            (append (list heap-rect corp-score-rect runner-score-rect) piles)
            (list cx cy u rx ry ru tw th colw x0 y-root slot-size col-x root-rect ice-rect
                  heap-rect corp-score-rect runner-score-rect))))

  ; ---- the drawing: a list of shapes for the projector
  (define items '())
  (define targets '())
  (define columns '())
  (define (draw! item) (set! items (cons item items)))
  (define (text! x y str kind . anchor)
    (draw! (list 'text x y str kind (if (null? anchor) 'start (car anchor)))))
  (define (rect! r kind) (draw! (list 'rect (list-ref r 0) (list-ref r 1) (list-ref r 2) (list-ref r 3) kind)))
  (define (line! x1 y1 x2 y2 kind) (draw! (list 'line x1 y1 x2 y2 kind)))
  ; a button someone can press by putting their wand (or the installer) down on it
  (define (button! id x y label owner)
    (let ((r (list x y (+ 18 (* 7 (string-length label))) 24)))
      (rect! r (if (eq? owner 'installer) 'target 'button))
      (text! (+ x 8) (+ y 16) label 'button)
      (set! targets (cons (list id r owner) targets))
      r))
  (define (target! id r owner) (set! targets (cons (list id r owner) targets)))

  (define (clicks-text n total)
    (let loop ((k 0) (acc ""))
      (if (= k total) acc (loop (+ k 1) (string-append acc (if (< k n) "● " "○ "))))))

  (define (build-ui! L)
    (set! items '())
    (set! targets '())
    (set! columns '())
    (let* ((G (list-ref L 4))
           (cx (list-ref G 0)) (cy (list-ref G 1)) (u (list-ref G 2))
           (rx (list-ref G 3)) (ry (list-ref G 4)) (ru (list-ref G 5))
           (tw (list-ref G 6)) (th (list-ref G 7))
           (y-root (list-ref G 10)) (slot-size (list-ref G 11))
           (col-x (list-ref G 12)) (root-rect (list-ref G 13)) (ice-rect (list-ref G 14))
           (corp-turn (and (eq? turn 'corp) (not run) (not winner)))
           (runner-turn (and (eq? turn 'runner) (not run) (not winner))))

      ; Corp status and basic actions, under the Corp identity
      (text! cx (+ cy u 18) (string-append "Corp · " (n->s corp-credits) "¢ · " (n->s corp-score) " pts") 'light)
      (text! cx (+ cy u 34) (string-append "R&D " (n->s rnd-count) " · HQ " (n->s hq-count)) 'small)
      (text! cx (+ cy u 50) (if (eq? turn 'corp) (string-append "your turn · " (clicks-text clicks 3)) "waiting") 'small)
      (if corp-turn
          (begin (button! '(gain corp) cx (+ cy (* 3.1 u)) "Gain 1¢" 'corp)
                 (button! '(draw corp) cx (+ cy (* 3.1 u) 30) "Draw 1" 'corp)))

      ; servers
      (let loop ((ss servers) (i 0))
        (if (pair? ss)
            (let* ((s (car ss))
                   (x (col-x i))
                   (ice (server-ice s))
                   (n (length ice))
                   (mid (+ x (/ slot-size 2))))
              (text! mid (- y-root 8) (server-name s) 'light 'middle)
              ; root
              (case (server-kind s)
                ((hq) (rect! (root-rect i) 'zone) (text! mid (+ y-root (/ slot-size 2)) "in hand" 'label 'middle))
                ((rnd) (rect! (root-rect i) 'zone) (text! mid (+ y-root (/ slot-size 2)) "stack" 'label 'middle))
                ((archives) (rect! (root-rect i) 'zone) (text! mid (+ y-root (/ slot-size 2)) "discards" 'label 'middle))
                ((remote)
                 (let ((root (server-root s)))
                   (if root
                       (begin
                         (draw-slot! root (root-rect i))
                         (target! (list 'select i) (root-rect i) 'select)
                         (target! (list 'slot i 'root) (root-rect i) 'installer)
                         (if (> (slot-counters root) 0)
                             (text! (+ x slot-size 6) (+ y-root 14)
                                    (string-append (n->s (slot-counters root)) " adv.") 'small)))
                       (if corp-turn
                           (let ((r (root-rect i)))
                             (rect! r 'target)
                             (text! mid (+ y-root (/ slot-size 2)) "+ install" 'small 'middle)
                             (target! (list 'install-root i) r 'installer)))))))
              ; ice, innermost first
              (let iloop ((k 1))
                (if (<= k n)
                    (let ((r (ice-rect i k)))
                      (draw-slot! (list-ref ice (- k 1)) r)
                      (target! (list 'slot i k) r 'installer)
                      (iloop (+ k 1)))))
              ; the installer's target for more ice
              (if (and corp-turn (< n max-ice))
                  (let ((r (list x (+ y-root (* (+ u 16) (+ n 1))) slot-size (* 0.55 u))))
                    (rect! r 'target)
                    (text! mid (+ (list-ref r 1) (* 0.35 u)) "+ ice" 'small 'middle)
                    (target! (list 'add-ice i) r 'installer)))
              ; where the run token can point
              (set! columns (cons (list i (list x y-root slot-size (* (+ u 16) (+ max-ice 2)))) columns))
              (loop (cdr ss) (+ i 1)))
            ; one more column: a new remote server
            (if corp-turn
                (let ((r (root-rect i)))
                  (rect! r 'target)
                  (text! (+ (col-x i) (/ slot-size 2)) (+ y-root (* 0.45 slot-size)) "+ new" 'small 'middle)
                  (text! (+ (col-x i) (/ slot-size 2)) (+ y-root (* 0.45 slot-size) 14) "remote" 'small 'middle)
                  (target! '(new-remote) r 'installer)))))

      ; the selected slot and what can be done with it
      (if (and selected corp-turn (slot-at (car selected) (cdr selected)))
          (let ((r (root-rect (car selected))))
            (rect! (list (- (list-ref r 0) 4) (- (list-ref r 1) 4) (+ slot-size 8) (+ slot-size 8)) 'ring)
            (button! '(advance) (+ (list-ref r 0) 2) (+ y-root slot-size -28) "Advance" 'corp)))
      (if (and remove-candidate (not run) (eq? turn 'corp)
               (slot-at (car remove-candidate) (cdr remove-candidate)))
          (let* ((key (cdr remove-candidate))
                 (r (if (eq? key 'root) (root-rect (car remove-candidate)) (ice-rect (car remove-candidate) key))))
            (rect! (list (- (list-ref r 0) 4) (- (list-ref r 1) 4) (+ slot-size 8) (+ slot-size 8)) 'warn)
            (button! '(confirm-remove) (list-ref r 0) (+ (list-ref r 1) slot-size 6) "Remove" 'corp)))

      ; the run
      (if run
          (let* ((i (run-server))
                 (x (col-x i))
                 (mid (+ x (/ slot-size 2)))
                 (phase (run-phase))
                 (s (list-ref servers i))
                 (bottom (+ y-root (* (+ u 16) (+ (length (server-ice s)) 1)))))
            (line! mid (+ bottom (* 1.2 u)) mid (+ y-root slot-size) 'path)
            (text! (+ x slot-size 10) (+ bottom (* 0.9 u)) (string-append "Run on " (server-name s)) 'light)
            (if (> (run-pos) 0)
                (let* ((r (ice-rect i (run-pos)))
                       (bx (+ (list-ref r 0) slot-size 12))
                       (by (list-ref r 1)))
                  (rect! (list (- (list-ref r 0) 5) (- (list-ref r 1) 5) (+ slot-size 10) (+ slot-size 10)) 'ring)
                  (case phase
                    ((approach)
                     (text! bx (+ by 14) "Corp: rez it by flipping it face up," 'light)
                     (text! bx (+ by 30) "or put your wand on Pass" 'small)
                     (button! '(pass-rez) bx (+ by 38) "Pass" 'corp))
                    ((encounter)
                     (let* ((card (slot-card (current-ice-slot)))
                            (data (cdr card))
                            (option (break-option data)))
                       (text! bx (+ by 12) (string-append (get 'label data) " · "
                                                          (n->s (get 'strength data 0)) " strength")
                              'light)
                       (let sloop ((subs (get 'subs data '())) (n 0) (y (+ by 20)))
                         (if (pair? subs)
                             (begin
                               (cond ((memv n (vector-ref run 3))
                                      (text! bx (+ y 16) (string-append "↳ " (sub-text (car subs)) " (broken)") 'small))
                                     (option
                                      (button! (list 'break n) bx y
                                               (string-append "↳ " (sub-text (car subs)) " · break: "
                                                              (list-ref option 1) " " (n->s (list-ref option 2)) "¢")
                                               'runner))
                                     (else
                                      (text! bx (+ y 16) (string-append "↳ " (sub-text (car subs)) " · nothing breaks it") 'small)))
                               (sloop (cdr subs) (+ n 1) (+ y 28)))
                             (button! '(fire) bx y "Let the rest fire" 'runner)))))
                    ((passed)
                     (button! '(continue) bx (+ by 4) "Continue" 'runner)
                     (button! '(jack-out) bx (+ by 32) "Jack out" 'runner))))
                ; at the server
                (let ((bx (+ x slot-size 12))
                      (by (+ y-root 8)))
                  (rect! (list (- x 5) (- y-root 5) (+ slot-size 10) (+ slot-size 10)) 'ring)
                  (if (eq? phase 'access)
                      (begin
                        (text! bx (+ by 12)
                               (case (server-kind s)
                                 ((hq) "Take a random card from the Corp's hand")
                                 ((rnd) "Take the top card of R&D")
                                 ((archives) "Nothing to access in Archives yet")
                                 (else (if (server-root s) (string-append "Take the card from " (server-name s))
                                           "Nothing is installed here")))
                               'light)
                        (if (or (eq? (server-kind s) 'archives) (and (eq? (server-kind s) 'remote) (not (server-root s))))
                            (button! '(nothing-to-access) bx (+ by 22) "Done" 'runner)
                            (text! bx (+ by 28) "and put it face up in the resolution box" 'small))))))))

      ; Runner status, basic actions and rig
      (text! (+ rx ru 12) (+ ry 16) (string-append "Runner · " (n->s runner-credits) "¢ · " (n->s runner-score) " pts") 'light)
      (text! (+ rx ru 12) (+ ry 32) (string-append "stack " (n->s stack-count) " · grip " (n->s grip-count)
                                                   " · MU " (n->s (mu-used)) "/4") 'small)
      (text! (+ rx ru 12) (+ ry 48) (if (eq? turn 'runner) (string-append "your turn · " (clicks-text clicks 4)) "waiting") 'small)
      (if runner-turn
          (begin (button! '(gain runner) rx (- ry (* 2.7 ru)) "Gain 1¢" 'runner)
                 (button! '(draw runner) rx (- ry (* 2.7 ru) 30) "Draw 1" 'runner)
                 (text! rx (- ry (* 2.7 ru) 40) "Run: point the run token at a server" 'small)))
      (rect! (list-ref L 1) 'zone)
      (text! (+ (list-ref (list-ref L 1) 0) 6) (+ (list-ref (list-ref L 1) 1) (list-ref (list-ref L 1) 3) 14) "rig" 'label)
      (for-each (lambda (r)
                  (let ((card (find (lambda (c) (equal? (car c) (vector-ref r 0))) cards-frame)))
                    (if card
                        (let ((x (list-ref card 3)) (y (list-ref card 4)) (data (vector-ref r 2)))
                          (cond ((get 'breaks data)
                                 (text! x (- y 6) (string-append "str " (n->s (get 'strength data 0))) 'small))
                                ((get 'holds data)
                                 (text! x (- y 6) (string-append (n->s (vector-ref r 3)) "¢ on card") 'small)
                                 (if (and runner-turn (> (vector-ref r 3) 0))
                                     (button! (list 'take (vector-ref r 0)) x (- y (* 2.6 ru))
                                              (string-append "Take " (n->s (get 'take data 0)) "¢") 'runner))))))))
                rig)
      (rect! (list-ref G 15) 'zone)
      (text! (+ (list-ref (list-ref G 15) 0) 4) (+ (list-ref (list-ref G 15) 1) (list-ref (list-ref G 15) 3) 14) "heap" 'label)

      ; score areas and the resolution box
      (rect! (list-ref G 16) 'zone)
      (text! (+ (list-ref (list-ref G 16) 0) 4) (+ (list-ref (list-ref G 16) 1) (list-ref (list-ref G 16) 3) 14) "Corp score area" 'label)
      (rect! (list-ref G 17) 'zone)
      (text! (+ (list-ref (list-ref G 17) 0) 4) (- (list-ref (list-ref G 17) 1) 6) "Runner score area" 'label)
      (let ((b (list-ref L 2)))
        (rect! b 'box)
        (text! (+ (list-ref b 0) (/ (list-ref b 2) 2)) (+ (list-ref b 1) (list-ref b 3) 16) "resolution box" 'label 'middle)
        (if box-note
            (text! (+ (list-ref b 0) (/ (list-ref b 2) 2)) (- (list-ref b 1) 10) box-note 'light 'end))
        (if (and run (eq? (run-phase) 'trash-choice))
            (let* ((card (find (lambda (c) (equal? (car c) box-card)) cards-frame))
                   (cost (if card (get 'trash (list-ref card 2) 0) 0)))
              (button! (list 'trash-access cost) (- (list-ref b 0) 150) (+ (list-ref b 1) 20)
                       (string-append "Trash · " (n->s cost) "¢") 'runner)
              (button! '(leave-access) (- (list-ref b 0) 150) (+ (list-ref b 1) 50) "Put it back" 'runner))))

      ; cards lying somewhere the table doesn't understand
      (for-each (lambda (c)
                  (if (eq? (alist-ref (car c) regions) 'loose)
                      (text! (list-ref c 3) (- (list-ref c 4) 6) "not a place the table knows" 'warn)))
                cards-frame)

      ; messages
      (if (and flash-text (< (date-now) flash-until))
          (text! (/ tw 2) (- th 16) flash-text 'warn 'middle))
      (if winner
          (text! (/ tw 2) (/ th 2) (string-append (side-name winner) " wins") 'big 'middle))))

  ; draw a slot: an outline for the card to lie in, face down until it is rezzed
  (define (draw-slot! slot r)
    (rect! r 'slot))

  ; ---- each tick
  (define cards-frame '())

  ; how many ticks each page has lain still
  (define stills '())

  (define (pointer-owners role)
    (case role
      ((installer) '(installer))
      ((corp-wand) '(corp select))
      ((runner-wand) '(runner))
      (else '())))

  (Claim this 'referee #t)

  (When ((this referee #t)) do
    (let ((cards cards-seen)
          (pointers pointers-seen)
          (ids ids-seen))
      (set! cards-seen '())
      (set! pointers-seen '())
      (set! ids-seen '())
      (set! cards-frame cards)
      (set! ids-frame ids)
      (let ((corp (layout-of 'corp))
            (runner (layout-of 'runner)))
        (if (not (and corp runner))
            (Claim this 'drawing
                   (list (list 'text 20 30 "Put both identity cards on the table to start" 'light 'start)))
            (let ((L (make-layout corp runner (table-size)))
                  (new-positions '())
                  (new-stills '()))
              ; a page counts as put down once it has lain still for two ticks
              (define (still! page x y)
                (let* ((last (alist-ref page last-positions))
                       (same (and last (< (abs (- (car last) x)) 0.5) (< (abs (- (cdr last) y)) 0.5)))
                       (count (if same (+ 1 (or (alist-ref page stills) 0)) 0)))
                  (set! new-positions (alist-set page (cons x y) new-positions))
                  (set! new-stills (alist-set page count new-stills))
                  (>= count 2)))
              ; cards put down somewhere new
              (for-each
               (lambda (c)
                 (let* ((page (list-ref c 0)) (x (list-ref c 3)) (y (list-ref c 4))
                        (cxm (+ x (/ (list-ref c 5) 2))) (cym (+ y (/ (list-ref c 6) 2))))
                   (if (still! page x y)
                       (let ((region (classify cxm cym L))
                             (old (alist-ref page regions)))
                         (if (not (equal? region old))
                             (begin
                               (if old (on-departure! page old))
                               (set! regions (alist-set page region regions))
                               (on-arrival! page (list-ref c 1) (list-ref c 2) region)))))))
               cards)
              ; cards taken off the table
              (for-each (lambda (p)
                          (if (not (find (lambda (c) (equal? (car c) (car p))) cards))
                              (begin (on-departure! (car p) (cdr p))
                                     (set! regions (alist-remove (car p) regions)))))
                        regions)
              ; wands, the installer and the run token: a pointer acts once, when it comes to
              ; rest somewhere new (put down, or turned). The layout changing underneath a
              ; pointer that lies still does nothing.
              (for-each
               (lambda (p)
                 (let* ((page (list-ref p 0)) (role (list-ref p 1))
                        (px (list-ref p 2)) (py (list-ref p 3))
                        (x (list-ref p 4)) (y (list-ref p 5))
                        (rest (alist-ref page rests)))
                   (if (and (still! page x y)
                            (not (and rest (< (abs (- (car rest) px)) 4) (< (abs (- (cdr rest) py)) 4))))
                       (begin
                         (set! rests (alist-set page (cons px py) rests))
                         (if (eq? role 'run-token)
                             (let ((column (find (lambda (col) (inside? px py (cadr col))) ui-columns)))
                               (if (and column (not winner)) (start-run! (car column))))
                             (let ((hit (find (lambda (t) (and (memq (list-ref t 2) (pointer-owners role))
                                                               (inside? px py (list-ref t 1))))
                                              ui-targets)))
                               (if hit (press! (car hit)))))))))
               pointers)
              (set! last-positions new-positions)
              (set! stills new-stills)
              (end-turn-if-done!)
              (build-ui! L)
              (set! ui-targets targets)
              (set! ui-columns columns)
              (Claim this 'drawing (reverse items)))))))

  (When ((?p card ?card)
         (?p (page left) ?x)
         (?p (page top) ?y)
         (?p (page width) ?w)
         (?p (page height) ?h))
   do (set! cards-seen (cons (list ?p (car ?card) (cadr ?card) ?x ?y ?w ?h) cards-seen)))

  (When ((?p pointer-role ?role)
         (?p pointer-at ?point)
         (?p (page left) ?x)
         (?p (page top) ?y))
   do (set! pointers-seen (cons (list ?p ?role (car ?point) (cdr ?point) ?x ?y) pointers-seen)))

  (When ((?p identity ?side)
         (?p (page left) ?x)
         (?p (page top) ?y)
         (?p (page width) ?w))
   do (set! ids-seen (cons (list ?side ?x ?y ?w) ids-seen)))
)))

; ----------------------------------------------------------------------------------------------
; The projector draws whatever the referee asks for.

(define projector (add-page (make-page-code
  (define (num n) (number->string (exact->inexact n)))

  (define (escape str)
    (let loop ((cs (string->list str)) (acc '()))
      (cond ((null? cs) (list->string (reverse acc)))
            ((char=? (car cs) #\&) (loop (cdr cs) (append (reverse (string->list "&amp;")) acc)))
            ((char=? (car cs) #\<) (loop (cdr cs) (append (reverse (string->list "&lt;")) acc)))
            (else (loop (cdr cs) (cons (car cs) acc))))))

  (define (svg-element! name attrs)
    (let ((svg (query-selector (get-element-by-id "table") "svg"))
          (elem (make-svg-element name)))
      (for-each (lambda (attr) (set-attribute! elem (car attr) (cdr attr))) attrs)
      (append-child! svg elem)
      elem))

  (define amber "#E8A317")
  (define amber-text "#9A6400")
  (define amber-fill "rgba(232,163,23,0.18)")

  (define (rect-style kind)
    (case kind
      ((slot) `(("fill" . "none") ("stroke" . ,amber) ("stroke-width" . "1.5") ("stroke-dasharray" . "5 4")))
      ((back) '(("fill" . "#2E5A9E") ("fill-opacity" . "0.85") ("stroke" . "#2E5A9E")))
      ((target) `(("fill" . ,amber-fill) ("stroke" . ,amber) ("stroke-width" . "1.2") ("stroke-dasharray" . "2 4")))
      ((button) `(("fill" . "rgba(232,163,23,0.35)") ("stroke" . ,amber) ("stroke-width" . "1.5")))
      ((ring) `(("fill" . "none") ("stroke" . ,amber) ("stroke-width" . "3")))
      ((warn) '(("fill" . "none") ("stroke" . "crimson") ("stroke-width" . "2.5")))
      ((box) `(("fill" . "none") ("stroke" . ,amber) ("stroke-width" . "2.5")))
      (else '(("fill" . "none") ("stroke" . "#9AA3A8") ("stroke-width" . "1.2") ("stroke-dasharray" . "4 4")))))

  (define (text-style kind)
    (case kind
      ((light) `(("fill" . ,amber-text) ("style" . "font: bold 13px sans-serif")))
      ((small) `(("fill" . ,amber-text) ("style" . "font: 12px sans-serif")))
      ((button) '(("fill" . "#3A2A00") ("style" . "font: bold 12px sans-serif")))
      ((warn) '(("fill" . "crimson") ("style" . "font: bold 13px sans-serif")))
      ((big) `(("fill" . ,amber-text) ("style" . "font: bold 34px sans-serif")))
      ((back) '(("fill" . "white") ("style" . "font: 11px sans-serif")))
      (else '(("fill" . "#6B7378") ("style" . "font: 12px sans-serif")))))

  (define (draw! item)
    (case (car item)
      ((rect)
       (svg-element! "rect" (append `(("x" . ,(num (list-ref item 1))) ("y" . ,(num (list-ref item 2)))
                                      ("width" . ,(num (list-ref item 3))) ("height" . ,(num (list-ref item 4)))
                                      ("rx" . "5"))
                                    (rect-style (list-ref item 5)))))
      ((line)
       (svg-element! "line" `(("x1" . ,(num (list-ref item 1))) ("y1" . ,(num (list-ref item 2)))
                              ("x2" . ,(num (list-ref item 3))) ("y2" . ,(num (list-ref item 4)))
                              ("stroke" . ,amber) ("stroke-width" . "3") ("stroke-dasharray" . "2 7")
                              ("stroke-linecap" . "round"))))
      ((text)
       (let ((elem (svg-element! "text" (append `(("x" . ,(num (list-ref item 1))) ("y" . ,(num (list-ref item 2)))
                                                  ("text-anchor" . ,(symbol->string (list-ref item 5))))
                                                (text-style (list-ref item 4))))))
         (set-inner-html! elem (escape (list-ref item 3)))))))

  (When ((?referee drawing ?items))
   do (for-each draw! ?items))
)))

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

(define (add-label pagediv text)
  (let ((div (make-element "div")))
    (set-attribute! div "class" "label")
    (append-child! div (make-text-node text))
    (append-child! pagediv div)))

(define (place! pid parent text left)
  (let ((div (get-page pid)))
    (append-child! parent div)
    (add-label div text)
    (set-style-left! div left)
    div))

(place! corp-identity system-pages "Corp identity" "4vw")
(place! runner-identity system-pages "Runner identity" "11vw")
; the Corp sits across the table, so its pointers start turned around, pointing down
(set-style-transform! (place! installer system-pages "installer" "18vw") "rotate(180deg)")
(set-style-transform! (place! corp-wand system-pages "Corp wand" "25vw") "rotate(180deg)")
(place! runner-wand system-pages "Runner wand" "32vw")
(place! run-token system-pages "run token" "39vw")
(place! whiskers system-pages "whiskers" "46vw")
(place! referee system-pages "referee" "53vw")
(place! projector system-pages "projector" "60vw")

(let loop ((cards corp-deck) (n 0))
  (if (pair? cards)
      (begin
        (place! (card-page (car (car cards)) (cdr (car cards))) corp-shelf
                (cadr (assq 'label (cdr (car cards))))
                (string-append (number->string (+ 4 (* n 7))) "vw"))
        (loop (cdr cards) (+ n 1)))))

(let loop ((cards runner-deck) (n 0))
  (if (pair? cards)
      (begin
        (place! (card-page (car (car cards)) (cdr (car cards))) runner-shelf
                (cadr (assq 'label (cdr (car cards))))
                (string-append (number->string (+ 4 (* n 7))) "vw"))
        (loop (cdr cards) (+ n 1)))))

(recalculate-pages)
