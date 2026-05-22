; Two-page live-coding playground. Each page is a draggable card with its
; own textarea. Hitting "Run" replaces that page's code via recode-page!:
; the engine retracts the old proc's claims/wishes/rules, swaps in the new
; proc, and (if the card is currently on the table) re-executes it under a
; fresh fixpoint.
;
; Eval'd code runs in (realtalk)'s scope, so the textareas can use Claim,
; Wish, When, get-page, set-background!, etc. directly — no imports.

(use-modules (scheme base)
             (dom js)
             (realtalk)
             (hoot ffi))

(define-foreign get-property
    "element" "getProperty"
    (ref null extern) (ref string) -> (ref string))
(define-foreign set-property!
    "element" "setProperty"
    (ref null extern) (ref string) (ref string) -> none)

(make-dynamic)

; Two pages whose initial bodies are no-ops. The textareas hold the code
; we actually want and we apply it on page load just below.
(define page1 (add-page (make-page-code #f)))
(define page2 (add-page (make-page-code #f)))

(define pages-div (get-element-by-id "pages"))
(define page1div  (get-page page1))
(define page2div  (get-page page2))
(append-child! pages-div page1div)
(append-child! pages-div page2div)

(define (add-label pagediv text)
  (let ((d (make-element "div")))
    (append-child! d (make-text-node text))
    (append-child! pagediv d)))
(add-label page1div "1")
(add-label page2div "2")

(set-style-left! page1div "30vw")
(set-style-left! page2div "40vw")

(define code1-area  (get-element-by-id "code1"))
(define code2-area  (get-element-by-id "code2"))
(define run1-button (get-element-by-id "run1"))
(define run2-button (get-element-by-id "run2"))
(define output      (get-element-by-id "output"))

(define (show str)
  (set-property! output "textContent" str))

; Read the textarea's source, hand it to recode-page!. If parsing or
; evaluation fails we render the error in the output strip instead of
; throwing out of the click handler.
(define (run-page! pid name area)
  (let ((src (get-property area "value")))
    (guard (exn (#t (show (format #f "error on ~a: ~a" name exn))))
      (recode-page! pid src)
      (show (format #f "~a updated" name)))))

(add-event-listener! run1-button "click"
  (procedure->external (lambda (e) (run-page! page1 "page 1" code1-area))))

(add-event-listener! run2-button "click"
  (procedure->external (lambda (e) (run-page! page2 "page 2" code2-area))))

; Apply the textareas' initial contents so the pages have meaningful code
; from the start. They won't *do* anything until dragged onto the table,
; but the engine has the procs registered and ready.
(run-page! page1 "page 1" code1-area)
(run-page! page2 "page 2" code2-area)

(show "Drag the pages onto the table. Edit a textarea and hit Run to recode.")
