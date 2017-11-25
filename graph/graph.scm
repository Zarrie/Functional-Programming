; Graph

(define (graph)
  (list 'graph))

(define (vertex? g a)
  (let ((v (assq a (cdr g))))
    (not (null? v))))

(define (neighbours? g a b)
  (let ((r1 (assq a (cdr g)))
        (r2 (assq b (cdr g))))
    (or (memq b (cdr r1)) (memq a (cdr r2)))))

(define (connect! g a b)
  (let ((v1 (assq a (cdr g)))
        (v2 (assq b (cdr g))))
    (begin
      (set-cdr! v2 (cons a (cdr v2)))
      (set-cdr! v1 (cons b (cdr v1))))))

(define (add-vertex! g . vl)
  (define (add-vertex-single! g v)
    (begin (set-cdr! g (cons (list v) (cdr g))) g))
  (define (helper lst res)
    (if (null? lst)
        res
        (helper (cdr lst) (add-vertex-single! res (car lst)))))
    (helper vl g))

(define (vertices g)
  (map car (cdr g)))

;(define g (graph))
;(add-vertex! g 'a)
;(add-vertex! g 'b)
;(connect! g 'a 'b)
;(add-vertex! g 'c 'd 'e 'f)
