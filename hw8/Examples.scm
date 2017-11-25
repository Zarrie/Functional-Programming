(define (memq? el lst)
  (cond
    ((null? lst) '())
    ((eq? el (car lst)) lst)
    (else (memq el (cdr lst)))))

(define (deep-found? el lst)
  (cond
    ((null? lst) #f)
    ((list? (car lst)) (or (deep-found? el (car lst)) (deep-found? el (cdr lst))))
    ((eq? el (car lst)) #t)
    (else (deep-found? el (cdr lst)))))


(define (derive f)
  (lambda (x)
    (let
        ((x0 (+ x 0.000000001)))
      (/ (- (f x) (f x0)) (- x x0)))))

(define (kth-der f k)
  (define (helper f k res)
    (if (= k 0)
        res
        (helper f (- k 1) (derive res))))
  (helper f k f))