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
  (define pi 3.14159265359)

  ; set t such that sin/cos are automatically phase-correct
  ; every <interval> milliseconds we rotate by <step> degrees
  ; so we can calculate at what <degrees> we would be when we enter
  (define t (date-now))
  (define interval 100)
  (define step 10)
  (define deg 
    (let* ((tmod (modulo t (inexact->exact (* (/ 360 step) interval))))
           (d (inexact->exact (* step (floor (/ tmod interval)))))) d))

  (define dx 20)
  (define maxlen 20)
  (define data '())

  (define (points)
    (let loop ((i (- maxlen 1)) (lst data) (acc '()))
      (if (null? lst)
        (reverse acc)
        (loop (- i 1) (cdr lst) (cons (cons (* i dx) (car lst)) acc)))))

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

  (define (add-data-point x)
    (if (< (length data) maxlen)
      (set! data (cons x data))
      (set! data (cons x (reverse (cdr (reverse data)))))))

  (When ((time now ,?t)) do
    (if (< interval (- ?t t)) (begin
      (set! deg (modulo (+ deg step) 360))
      (let ((rad (* (/ deg 180) pi)))
        (add-data-point (+ (inexact->exact (round (* (sin rad) 200))) 200)))
      (set! t (+ t interval))))
    ; imported from d3. selects #table.svg from javascript atm
    (let ((line (catmull-rom (points->arr (points)))))
      (set-attribute! line "style" "stroke: red; fill: none; stroke-width: 2;")))

)))

(define page2 (add-page (make-page-code
  (define pi 3.14159265359)

  ; set t such that sin/cos are automatically phase-correct
  ; every <interval> milliseconds we rotate by <step> degrees
  ; so we can calculate at what <degrees> we would be when we enter
  (define t (date-now))
  (define interval 100)
  (define step 10)
  (define deg 
    (let* ((tmod (modulo t (inexact->exact (* (/ 360 step) interval))))
           (d (inexact->exact (* step (floor (/ tmod interval)))))) d))

  (define dx 20)
  (define maxlen 20)
  (define data '())

  (define (points)
    (let loop ((i (- maxlen 1)) (lst data) (acc '()))
      (if (null? lst)
        (reverse acc)
        (loop (- i 1) (cdr lst) (cons (cons (* i dx) (car lst)) acc)))))

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

  (define (add-data-point x)
    (if (< (length data) maxlen)
      (set! data (cons x data))
      (set! data (cons x (reverse (cdr (reverse data)))))))

  (When ((time now ,?t)) do
    (if (< interval (- ?t t)) (begin
      (set! deg (modulo (+ deg step) 360))
      (let ((rad (* (/ deg 180) pi)))
        (add-data-point (+ (inexact->exact (round (* (cos rad) 200))) 200)))
      (set! t (+ t interval))))
    ; imported from d3. selects #table.svg from javascript atm
    (let ((line (catmull-rom (points->arr (points)))))
      (set-attribute! line "style" "stroke: blue; fill: none; stroke-width: 2;")))

)))

(define (add-text pagediv text)
  (let ((div (make-element "div")))
    (append-child! div (make-text-node text))
    (append-child! pagediv div)))
(define page1div (get-page page1))
(append-child! pages page1div)
(add-text page1div "sin")
(set-style-left! page1div "30vw")
(define page2div (get-page page2))
(append-child! pages page2div)
(add-text page2div "cos")
(set-style-left! page2div "40vw")
