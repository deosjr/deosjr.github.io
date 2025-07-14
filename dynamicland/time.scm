(use-modules (dom js)
             (datalog)
             (realtalk)
             (hoot hashtables)
             (hoot ffi))

(add-event-listener! (window) "update-realtalk" (procedure->external (lambda (e)
  (recalculate-pages))))

(define pages (get-element-by-id "pages"))

(make-dynamic)

(define page1 (add-page (make-page-code
  (When ((time now ,?t)) do
    (let* ((mod (modulo ?t 5000))
           (x (* 360 (/ mod 5000.0))))
      (set-background! (get-page this) (format #f "hsl(~a, 100%, 50%)" x ))))
)))

(define (add-text pagediv text)
  (let ((div (make-element "div")))
    (append-child! div (make-text-node text))
    (append-child! pagediv div)))
(define page1div (get-page page1))
(append-child! pages page1div)
(add-text page1div "#1")
(set-style-left! page1div "30vw")
