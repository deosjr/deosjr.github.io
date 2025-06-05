(use-modules (dom js)
             (realtalk))

(define dynamicland (append-child! (get-element-by-id "table") (make-element "div")))
(set-attribute! dynamicland "id" "dynamicland")
(define papers (get-element-by-id "papers"))

;(define *testvar* 0)

(define page1 (add-page (lambda (this) 
  ; this is an example of doing state over time the _wrong_ way, but it does work
  ;(set! *testvar* (+ 1 *testvar*))
  ;(append-child! (get-page this) (make-text-node (format #f "~a" *testvar*)))
  ; the right way would be to declare *testvar* in this scope, but that currently _doesnt_ work
  (Claim this 'highlighted "red"))))

(define page2 (add-page (lambda (this) 
  ; confusing: conditions need logic vars to be unquoted, code does _not_
  ; confusing: reordering tuple in condition/statement is unhelpful
  ; page-local state: this var is now live as long as this page is on a table
  ;(define *testvar* 0)
  ;(When ((highlighted ,?p ,?color)) do (append-child! (document-body) (make-text-node (format #f "VAR:~a" *testvar*))) (set! *testvar* (+ 1 *testvar*)) (set-background! (get-page ?p) ?color)))))
  (When ((highlighted ,?p ,?color)) do (set-background! (get-page ?p) ?color)))))

(define page1div (get-page page1))
(append-child! papers page1div)
(define page2div (get-page page2))
(append-child! papers page2div)
(define (add-text pagediv text)
  (let ((div (make-element "div")))
    (append-child! div (make-text-node text))
    (append-child! pagediv div)))
(add-text page1div "(Claim this 'highlighted \"red\")")
(add-text page2div "(When ((highlighted ,?p ,?color)) do (set-background! (get-page ?p) ?color))")

(recalculate-pages)

(define *i* 1)
(for-each (lambda (p) 
  (let* ((page (get-page p))
         (left-string (get-left page))
         (str-len (string-length left-string))
         (left (if (= str-len 0) 0
                 (string->number (substring left-string 0 (- str-len 2)))))
         (newleft (format #f "~apx" (+ left (* *i* 50)))))
    (set! *i* (+ *i* 1))
    (set-style-left! page newleft))) (get-pages))
