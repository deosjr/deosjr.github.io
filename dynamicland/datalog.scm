(use-modules (dom js)
             (minikanren)
             (datalog))

(define dl (make-new-datalog))

(define a (dl-record! dl 'vertex))
(define b (dl-record! dl 'vertex))
(define c (dl-record! dl 'vertex))
(define d (dl-record! dl 'vertex))
(define e (dl-record! dl 'vertex))

(define (dl-edge x y) (dl-assert! dl x 'edge y))
(dl-edge a c)
(dl-edge b a)
(dl-edge b d)
(dl-edge c d)
(dl-edge d a)
(dl-edge d e)

(dl-rule! dl (reachable ,?x ,?y) :- (edge ,?x ,?y))
(dl-rule! dl (reachable ,?x ,?y) :- (edge ,?x ,?z) (reachable ,?z ,?y))

(dl-fixpoint! dl)
;(define query (dl-find ,?id where (,?id reachable ,?id)))
(define query (dl-find (fresh-vars 1 (lambda (?id) (dl-findo dl ( (,?id reachable ,?id) ))))))

(append-child! (get-element-by-id "dl-output") (make-text-node (format #f "~a" query)))
