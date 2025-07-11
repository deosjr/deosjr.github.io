(use-modules (dom js)
             (realtalk)
             (hoot ffi))

(add-event-listener! (window) "update-realtalk" (procedure->external (lambda (e)
  (recalculate-pages))))

(define pages (get-element-by-id "pages"))

(make-dynamic)

(define page1 (add-page (make-page-code
  ; todo: key buffer that modules/realtalk can update somehow, if page=keyboard?
  (define last-key #f)

  (Claim this 'keyboard #t) ; todo: value is keyboard identifier

  (When ((pressed keyboard ,?key)) do (set! last-key ?key))

  (When ((keyboard ,this #t)
         ((page left) ,this ,?x)
         ((page top) ,this ,?y)
         ((page width) ,this ,?w)
         ((page height) ,this ,?h)) do
    (if last-key
      (let* ((table-div (get-element-by-id "table"))
             (svg (query-selector table-div "svg"))
             (text (make-svg-element "text")))
        (set-inner-html! text (format #f "~a" last-key))
        (set-attribute! text "x" (number->string (exact->inexact (+ ?x (/ ?w 2)))))
        (set-attribute! text "y" (number->string (exact->inexact (+ ?y (/ ?h 2)))))
        (set-attribute! text "style" "font: bold 60px sans-serif;fill:purple;opacity:0.8")
        (append-child! svg text))))
)))

(define page1div (get-page page1))
(append-child! pages page1div)
(define (add-text pagediv text)
  (let ((div (make-element "div")))
    (append-child! div (make-text-node text))
    (append-child! pagediv div)))
(add-text page1div "#1")
(set-style-left! page1div "30vw")

(recalculate-pages)
