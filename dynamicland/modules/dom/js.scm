(define-module (dom js)
  #:use-module (hoot ffi)
  #:export (document-body
            remove-element
            get-element-by-id
            make-text-node
            append-child!
            make-element
            set-attribute!
            set-background!
            add-event-listener!
            mouse-x
            mouse-y
            offset-left
            offset-top
            set-style!
            set-style-left!
            set-style-top!
            set-style-transform!
            set-z-index!
            set-position!
            get-left
            get-top
            get-transform
            get-z-index
            get-position
            get-x
            get-y
            get-width
            get-height
            get-bounding-client-rect
            prevent-default
            first-touch
            focus
            get-key))

(define-foreign document-body
    "document" "body"
    -> (ref null extern))
(define-foreign get-element-by-id
    "document" "getElementById"
    (ref string) -> (ref null extern))
(define-foreign make-text-node
    "document" "createTextNode"
    (ref string) -> (ref null extern))
(define-foreign remove-element
    "element" "removeElement"
    (ref null extern) -> none)
(define-foreign append-child!
    "element" "appendChild"
    (ref null extern) (ref null extern) -> (ref null extern))
(define-foreign make-element
    "document" "createElement"
    (ref string) -> (ref null extern))
(define-foreign set-attribute!
    "element" "setAttribute"
    (ref null extern) (ref string) (ref string) -> none)
(define-foreign set-background!
    "element" "setBackground"
    (ref null extern) (ref string) -> none)
(define-foreign add-event-listener!
  "element" "addEventListener"
  (ref null extern) (ref string) (ref null extern) -> none)
(define-foreign mouse-x
  "event" "mouseX"
  (ref null extern) -> i32)
(define-foreign mouse-y
  "event" "mouseY"
  (ref null extern) -> i32)
(define-foreign offset-left
  "element" "offsetLeft"
  (ref null extern) -> i32)
(define-foreign offset-top
  "element" "offsetTop"
  (ref null extern) -> i32)
(define-foreign set-style!
  "element" "setStyle"
  (ref null extern) (ref string) -> none)
(define-foreign set-style-left!
  "element" "setLeft"
  (ref null extern) (ref string) -> none)
(define-foreign set-style-top!
  "element" "setTop"
  (ref null extern) (ref string) -> none)
(define-foreign set-style-transform!
  "element" "setTransform"
  (ref null extern) (ref string) -> none)
(define-foreign set-z-index!
  "element" "setZIndex"
  (ref null extern) (ref string) -> none)
(define-foreign set-position!
  "element" "setPosition"
  (ref null extern) (ref string) -> none)
(define-foreign get-left
  "element" "getLeft"
  (ref null extern) -> (ref string))
(define-foreign get-top
  "element" "getTop"
  (ref null extern) -> (ref string))
(define-foreign get-transform
  "element" "getTransform"
  (ref null extern) -> (ref string))
(define-foreign get-z-index
  "element" "getZIndex"
  (ref null extern) -> (ref string))
(define-foreign get-position
  "element" "getPosition"
  (ref null extern) -> (ref string))
(define-foreign get-x
  "element" "getX"
  (ref null extern) -> i32)
(define-foreign get-y
  "element" "getY"
  (ref null extern) -> i32)
(define-foreign get-width
  "element" "getWidth"
  (ref null extern) -> i32)
(define-foreign get-height
  "element" "getHeight"
  (ref null extern) -> i32)
(define-foreign get-bounding-client-rect
  "element" "getBoundingClientRect"
  (ref null extern) -> (ref null extern))
(define-foreign prevent-default
  "event" "preventDefault"
  (ref null extern) -> none)
(define-foreign first-touch
  "event" "firstTouch"
  (ref null extern) -> (ref null extern))
(define-foreign focus
  "element" "focus"
  (ref null extern) -> none)
(define-foreign get-key
  "event" "getKey"
  (ref null extern) -> (ref string))
