(use-modules (dom js)
             (datalog)
             (realtalk)
             (hoot ffi)
             (hoot hashtables))

; todo: write about how extending 'pointing at things' was made easy
; by having existing whisker page claim 'you point at this coordinate'
; it could be reused as-is and 'you point at a link' can be on the wiki page

(make-dynamic)

(define-foreign wiki-html
    "wiki" "html"
    (ref string) -> (ref string))
(define-foreign parse-dom
    "document" "parseDOM"
    (ref string) -> (ref null extern))
(define-foreign get-property
    "element" "getProperty"
    (ref null extern) (ref string) -> (ref string))
(define-foreign set-property!
    "element" "setProperty"
    (ref null extern) (ref string) (ref string) -> none)
(define-foreign get-attribute
    "element" "getAttribute"
    (ref null extern) (ref string) -> (ref string))
(define-foreign query-selector-all
    "element" "querySelectorAll"
    (ref null extern) (ref string) -> (ref null extern))
(define-foreign array-length
    "array" "length"
    (ref null extern) -> i32)
(define-foreign array-ref
    "array" "ref"
    (ref null extern) i32 -> (ref null extern))

(define (arr->list arr) 
  (let loop ((i 0) (len (array-length arr)) (acc '()))
    (if (= i len)
        (reverse acc)
        (loop (+ i 1) len (cons (array-ref arr i) acc)))))

(define pages (get-element-by-id "pages"))

; this event serves as a callback from async fetch API
(add-event-listener! (window) "urlfetched" (procedure->external (lambda (e)
  (recalculate-pages))))

(add-event-listener! (get-element-by-id "topic") "change" (procedure->external (lambda (e)
  (recalculate-pages))))

; todo: get the title from an html input field for now?
(define page1 (add-page (make-page-code
  (define urlpref "https://en.wikipedia.org/api/rest_v1/page/html/" )

  (define (claim-link-dimensions a)
    (let* ((rect (get-bounding-client-rect a))
           (table-rect (get-bounding-client-rect (get-element-by-id "table")))
           (x (get-x rect))
           (y (get-y rect))
           (w (get-width rect))
           (h (get-height rect))
           (tx (get-x table-rect))
           (ty (get-y table-rect))
           (args `(,(- x tx) ,(- y ty) ,w ,h)))
      (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,a link-dimensions ,args)) #t)
      (hashtable-set! (datalog-idb (get-dl)) `(,a link-dimensions ,args) #t)
      (Claim a 'link-dimensions args)))

  (define (claim-wiki-text page x y w topic)
    (let ((args `(,x ,y ,w ,topic)))
      (hashtable-set! (datalog-idb (get-dl)) `(,this claims (,page wiki ,args)) #t)
      (hashtable-set! (datalog-idb (get-dl)) `(,page wiki ,args) #t)
      (Claim page 'wiki args)))

  ; always true
  (When (((page left) ,this ,?x)
         ((page top) ,this ,?y)
         ((page width) ,this ,?w))
   do (let ((topic (get-property (get-element-by-id "topic") "value")))
        (claim-wiki-text this ?x ?y ?w topic)))

  (When ((wiki ,?p (,?x ,?y ,?w ,?topic))) do
    (let* ((text-div (make-element "div"))
           (table-div (get-element-by-id "table"))
           (other-div (query-selector table-div "other"))
           (p (make-element "p"))
           (url (string-append urlpref ?topic))
           ; Parsoid wraps the lead in <section data-mw-section-id="0">.
           ; Its first few direct <p> children are <p class="mw-empty-elt">
           ; — placeholders for transclusion machinery — followed by the
           ; real lead paragraph. The lead consistently has no class
           ; attribute (just an id), so :not([class]) is the discriminator.
           ; The direct-child `>` keeps us out of infoboxes/templates
           ; nested inside the section.
           (parsed (parse-dom (wiki-html url)))
           (dom (query-selector parsed "section[data-mw-section-id=\"0\"] > p:not([class])"))
           (html (if (external-null? dom) "" (get-property dom "innerHTML"))))
      (set-attribute! text-div "class" "text-projection")
      (set-style-left! text-div (format #f "~apx" (+ ?x ?w 10)))
      (set-style-top! text-div (format #f "~apx" ?y))
      (set-property! p "innerHTML" html)
      (append-child! text-div p)
      (append-child! other-div text-div)
      (if (not (external-null? dom))
        (for-each (lambda (link)
          (claim-link-dimensions link))
          (arr->list (query-selector-all text-div "a"))))))

  ; The wiki article slug for a rendered link, or #f if the link points
  ; somewhere we can't fetch via the REST API.
  ;
  ; MediaWiki returns article references as <a href="./Article_Title">. The
  ; slug is exactly what the API wants — already URL-encoded, with the right
  ; case and underscores. The link's visible text (innerHTML) is the
  ; *display* form, which usually differs (lowercased, spaces instead of
  ; underscores, sometimes wrapped in <i> or <span> markup). Using innerHTML
  ; happens to work for short single-word links and silently 404s on
  ; everything else.
  ;
  ; We also skip:
  ;   - external links (href doesn't start with "./")
  ;   - fragment-only links (href starts with "#")
  (define (link->topic link)
    (let ((href (get-attribute link "href")))
      (cond ((and (>= (string-length href) 2)
                  (string=? (substring href 0 2) "./"))
             (substring href 2 (string-length href)))
            (else #f))))

  (When ((points-at ,?p ,?link)
         ((page left) ,?p ,?x)
         ((page top) ,?p ,?y)
         ((page width) ,?p ,?w))
   do (let ((topic (link->topic ?link)))
        (when topic
          (set-background! ?link "hotpink")
          (claim-wiki-text ?p ?x ?y ?w topic))))
)))

; whiskers. see whiskers.scm
; changed to only care about pointing at links
(define page2 (add-page (make-page-code
  (Wish this 'has-whiskers #t)

  (When ((wishes ,?p (,?p has-whiskers ,#t))) do
    (Claim ?p 'has-whiskers #t))
  (When ((has-whiskers ,?p #t)) do
    (add-class! (get-page ?p) "whisker"))

  (When ((has-whiskers ,?p #t)
         ((page left) ,?p ,?x)
         ((page top) ,?p ,?y)
         ((page width) ,?p ,?width))
	; TODO: angle?
   do (let* ((w (/ ?width 2))
             (px (+ ?x w))
             (py (- ?y 50)))
         (Claim ?p 'pointer-at (cons px py)) ))

  (When ((pointer-at ,?p ,?point)
         (link-dimensions ,?q (,?qx ,?qy ,?qw ,?qh)))
   do (let ((px (car ?point))
            (py (cdr ?point)))
        (if (and (> px ?qx)
                 (< px (+ ?qx ?qw))
                 (> py ?qy)
                 (< py (+ ?qy ?qh)))
           (Claim ?p 'points-at ?q))))
)))

; extra pointer page
(define page3 (add-page (make-page-code
  (Wish this 'has-whiskers #t)
)))

(define page1div (get-page page1))
(append-child! pages page1div)
(define page2div (get-page page2))
(append-child! pages page2div)
(define page3div (get-page page3))
(append-child! pages page3div)
(define (add-text pagediv text)
  (let ((div (make-element "div")))
    (append-child! div (make-text-node text))
    (append-child! pagediv div)))
(add-text page1div "source")
(add-text page2div "whiskers")
(add-text page3div "extra pointer")
(set-style-left! page1div "30vw")
(set-style-left! page2div "40vw")
(set-style-left! page3div "50vw")

(recalculate-pages)
