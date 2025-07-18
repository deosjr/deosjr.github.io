(use-modules (dom js)
             (datalog)
             (realtalk)
             (hoot hashtables)
             (hoot ffi))

(define-foreign catmull-rom
    "d3" "catmullRom"
    (ref null extern) -> (ref null extern))
(define-foreign make-array
    "array" "make"
    i32 -> (ref null extern))
(define-foreign array-set-arr!
    "array" "set"
    (ref null extern) i32 (ref null extern) -> none)
(define-foreign array-set-n!
    "array" "set"
    (ref null extern) i32 i32 -> none)

(add-event-listener! (window) "update-realtalk" (procedure->external (lambda (e)
  (recalculate-pages))))

(define pages (get-element-by-id "pages"))

(make-dynamic)

(define page1 (add-page (make-page-code
  (define (js-point p)
    (let ((arr (make-array 2))
          (x (car p))
          (y (cdr p)))
      (array-set-n! arr 0 x)
      (array-set-n! arr 1 y) arr))

  (define (points->arr p) 
    (let ((arr (make-array (length p))))
      (let loop ((i 0)  (lst p))
        (if (null? lst) arr
          (begin
            (array-set-arr! arr i (js-point (car lst)))
            (loop (+ i 1) (cdr lst)))))))

  ; horrible hack :(
  ; this is still slow because of a bug in modules/realtalk
  ; but at least doesnt create ~10 svg paths each iteration now
  ; the actual fix is bumping the hash recursion value in (hoot hashtables)
  (define m #f)
  (When ((time now ,?t)) do
    (set! m (make-hashtable hash equal?)))

  (When ((wishes ,?p (,?p points-drawn (,?points ,?color)))) do
    (let ((pstr (format #f "~a" ?points)))
    (if (not (hashtable-ref m pstr #f)) (begin
      (hashtable-set! m pstr #t)
    ; imported from d3. selects #table.svg from javascript atm
    (let ((line (catmull-rom (points->arr ?points))))
      (set-attribute! line "style" (format #f "stroke: ~a; fill: none; stroke-width: 2;" ?color))))
  )))

)))

; plots the total production MW of the grid
(define page2 (add-page (make-page-code
  (define dx 20)
  (define maxlen 20)
  (define production '())
  (define consumption '())

  (define (points data)
    (let loop ((i (- maxlen 1)) (lst data) (acc '()))
      (if (null? lst)
        (reverse acc)
        (loop (- i 1) (cdr lst) (cons (cons (* i dx) (car lst)) acc)))))

  (define (add-data-point data x)
    (if (< (length data) maxlen)
      (cons x data)
      (cons x (reverse (cdr (reverse data))))))

  (define (wish-points-drawn p color)
    (hashtable-set! (datalog-idb (get-dl)) `(,this wishes (,this points-drawn (,p ,color))) #t)
    (Wish this 'points-drawn `(,p ,color)))

  (When ((production-total-mw ,?grid ,?mw)) do
    ; max is 300, we want 0 to be down
    (let ((data (add-data-point production (- 330 ?mw))))
      (set! production data)
      (wish-points-drawn (points data) "green")))

  ; todo: for now we assume a single consumer
  (When ((consumes ,?village ,?mw)) do
    ; max is 300, we want 0 to be down
    (let ((data (add-data-point consumption (- 330 ?mw))))
      (set! consumption data)
      (wish-points-drawn (points data) "red")))
)))

; plots frequency of the grid
(define page3 (add-page (make-page-code
  (define dx 20)
  (define maxlen 20)
  (define frequency '())

  (define (points data)
    (let loop ((i (- maxlen 1)) (lst data) (acc '()))
      (if (null? lst)
        (reverse acc)
        (loop (- i 1) (cdr lst) (cons (cons (* i dx) (car lst)) acc)))))

  (define (add-data-point data x)
    (if (< (length data) maxlen)
      (cons x data)
      (cons x (reverse (cdr (reverse data))))))

  (define (wish-points-drawn p color)
    (hashtable-set! (datalog-idb (get-dl)) `(,this wishes (,this points-drawn (,p ,color))) #t)
    (Wish this 'points-drawn `(,p ,color)))

  (When ((grid-frequency ,?grid ,?hz)) do
    (let* ((freq (inexact->exact (round (* 100 ?hz))))
           (data (add-data-point frequency (- 5300 freq))))
      (set! frequency data)
      (wish-points-drawn (points data) "blue")))
)))

(define generator1 (add-page (make-page-code
  (Claim this 'generates 100) ; MW
)))
(define generator2 (add-page (make-page-code
  (Claim this 'generates 100) ; MW
)))
(define generator3 (add-page (make-page-code
  (Claim this 'generates 100) ; MW
)))

(define village (add-page (make-page-code
  ; consumes MW variable over time
  ; todo: t is in millis and jumps per ~100 or so, making this look weird
  ; but thats mostly fine, we just want some random variation
  (define (claim-consumption mw)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,this consumes ,mw)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,this consumes ,mw) #t)
    (Claim this 'consumes mw))

  (When ((time now ,?t)) do
    (claim-consumption (inexact->exact (round (+ 300 (* 20 (sin (* 0.1 ?t))))))))
)))

; todo: sum and claim facts about the consumed MW in the same way
; todo: claim facts about the grid frequency based on production/consumption/previous delta freq
; note: this wish/claim setup means the grid lags one iteration behind, always!
(define grid (add-page (make-page-code
  (define f0 50)
  (define freq 50.0)
  (define df 0)
  (define dampingfactor 100)
  (define inertiaM 5000)
  (define productionTotalMW 0)
  (define consumptionTotalMW 0)

  ; we start this rule with production/consumption summed in previous iteration
  (When ((time now ,?t)) do
    (claim-production-mw)
    (calculate-frequency-deviation)
    (claim-frequency)
    (set! consumptionTotalMW 0)
    (set! productionTotalMW 0))

  (define (calculate-frequency-deviation)
    (let* ((powerdelta (- productionTotalMW consumptionTotalMW))
           (ddf (exact->inexact (/ (- powerdelta (* dampingfactor df)) inertiaM)))
           (newdf (+ df ddf))
           (newfreq (+ f0 newdf)))
      (set! df newdf)
      (set! freq newfreq)))

  (define (claim-frequency)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,this grid-frequency ,freq)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,this grid-frequency ,freq) #t)
    (Claim this 'grid-frequency freq))

  (define (claim-production-mw)
    (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,this production-total-mw ,productionTotalMW)) #t)
    (hashtable-set! (datalog-idb (get-dl)) `(,this production-total-mw ,productionTotalMW) #t)
    (Claim this 'production-total-mw productionTotalMW))

  (define (wish-production-updates g mw)
    (hashtable-set! (datalog-idb (get-dl)) `(,this wishes (,this updates-production-mw (,g ,mw))) #t)
    (Wish this 'updates-production-mw `(,g ,mw)))

  ; using the wish guarantees claiming previous productionTotal goes first
  ; then productionTotal is reset to 0 and all wishes add up 
  (When ((generates ,?g ,?MW)) do
    (wish-production-updates ?g ?MW))

  ; we have to claim ?g though we dont use it so wishes are unique!
  (When ((wishes ,this (,this updates-production-mw (,?g ,?MW)))) do
    (set! productionTotalMW (+ productionTotalMW ?MW)))

  (When ((consumes ,?v ,?mw)) do
    (set! consumptionTotalMW ?mw))

)))

(define (add-text pagediv text)
  (let ((div (make-element "div")))
    (append-child! div (make-text-node text))
    (append-child! pagediv div)))
(define page1div (get-page page1))
(append-child! pages page1div)
(add-text page1div "graph")
(set-style-left! page1div "30vw")
(define page2div (get-page page2))
(append-child! pages page2div)
(add-text page2div "production MW")
(set-style-left! page2div "40vw")
(define page3div (get-page page3))
(append-child! pages page3div)
(add-text page3div "frequency")
(set-style-left! page3div "50vw")

(define g1 (get-page generator1))
(append-child! pages g1)
(add-text g1 "generator")
(set-style-left! g1 "60vw")
(define g2 (get-page generator2))
(append-child! pages g2)
(add-text g2 "generator")
(set-style-left! g2 "70vw")
(define g3 (get-page generator3))
(append-child! pages g3)
(add-text g3 "generator")
(set-style-left! g3 "80vw")

(define v1 (get-page village))
(append-child! pages v1)
(add-text v1 "village")
(set-style-left! v1 "10vw")

(define griddiv (get-page grid))
(append-child! pages griddiv)
(add-text griddiv "grid")
(set-style-left! griddiv "20vw")
