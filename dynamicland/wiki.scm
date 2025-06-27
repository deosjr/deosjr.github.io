(use-modules (dom js)
             (datalog)
             (realtalk)
             (hoot ffi)
             (hoot hashtables))

(define-foreign window
    "window" "window"
    -> (ref null extern))
(define-foreign wiki-html
    "wiki" "html"
    (ref string) -> (ref string))
(define-foreign parse-dom
    "document" "parseDOM"
    (ref string) -> (ref null extern))
(define-foreign get-property
    "element" "getProperty"
    (ref null extern) (ref string) -> (ref string))

(define pages (get-element-by-id "pages"))

; this event serves as a callback from async fetch API
(add-event-listener! (window) "urlfetched" (procedure->external (lambda (e)
  (recalculate-pages))))

(add-event-listener! (get-element-by-id "topic") "change" (procedure->external (lambda (e)
  (recalculate-pages))))

; todo: get the title from an html input field for now?
(define page1 (add-page (make-page-code
  (define urlpref "https://en.wikipedia.org/api/rest_v1/page/html/" )

  (When (((page left) ,this ,?x)
         ((page top) ,this ,?y)
         ((page width) ,this ,?w)) do
    (let* ((text-div (make-element "div"))
           (table-div (get-element-by-id "table"))
           (p (make-element "p"))
           (url (string-append urlpref (get-property (get-element-by-id "topic") "value")))
           ; by experimentation: this is the first paragraph when returned by wiki REST API
           (dom (query-selector (parse-dom (wiki-html url)) "style ~ p:not([class])" ))
           (html (if (external-null? dom) "" (get-property dom "innerHTML"))))
      (set-attribute! text-div "class" "text-projection")
      (set-style-left! text-div (format #f "~apx" (+ ?x ?w 10)))
      (set-style-top! text-div (format #f "~apx" ?y))
      (append-child! p (make-text-node html))
      (append-child! text-div p)
      (append-child! table-div text-div)))
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
