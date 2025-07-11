(use-modules (dom js)
             (realtalk))

(define pages (get-element-by-id "pages"))

(make-dynamic)

;(define *testvar* 0)

(define page1 (add-page (make-page-code
  ; this is an example of doing state over time the _wrong_ way, but it does work
  ;(set! *testvar* (+ 1 *testvar*))
  ;(append-child! (get-page this) (make-text-node (format #f "~a" *testvar*)))
  ; the right way would be to declare *testvar* in this scope, but that currently _doesnt_ work
  (Claim this 'highlighted "yellow"))))

(define page2 (add-page (make-page-code
  ; confusing: conditions need logic vars to be unquoted, code does _not_
  ; confusing: reordering tuple in condition/statement is unhelpful
  ; page-local state: this var is now live as long as this page is on a table
  ;(define *testvar* 0)
  ;(When ((highlighted ,?p ,?color)) do (append-child! (document-body) (make-text-node (format #f "VAR:~a" *testvar*))) (set! *testvar* (+ 1 *testvar*)) (set-background! (get-page ?p) ?color)))))
  (When ((highlighted ,?p ,?color)
         ((page left) ,this ,?thisleft)
         ((page left) ,?p ,?thatleft))
   do (if (and (< ?thisleft ?thatleft)
               (< 100 (- ?thatleft ?thisleft)))
        (set-background! (get-page ?p) ?color))))))

(define page1div (get-page page1))
(append-child! pages page1div)
(define page2div (get-page page2))
(append-child! pages page2div)
(define (add-text pagediv text)
  (let ((div (make-element "div")))
    (append-child! div (make-text-node text))
    (append-child! pagediv div)))
(add-text page1div "#1")
(add-text page2div "#2")
(set-style-left! page1div "30vw")
(set-style-left! page2div "40vw")

; bonus page! cycles through colors based on rotation
(define page3 (add-page (make-page-code
  (Claim this 'prismatic #t)
  (When ((prismatic ,?p #t)
         ((page rotation) ,?p ,?degrees))
   do (set-background! (get-page ?p) (format #f "hsl(~a, 100%, 50%)" ?degrees ))))))
(define page3div (get-page page3))
(append-child! pages page3div)
(set-style-left! page3div "50vw")
(add-text page3div "#3")

(recalculate-pages)
