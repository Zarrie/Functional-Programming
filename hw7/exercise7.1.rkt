; returns new list with the squares of the elements of the given list
(define (squares l)
  (map (lambda (x) (* x x)) l))


; same as squares but takes list of lists
(define (squares-2 l)
  (map (lambda (sublist) (map (lambda (x) (* x x))
                              sublist))
       l))

; filters elements of list l to only these ones for whom pred(element) = #t
(define (filter pred l)
  (define (helper pred lst res)
    (cond
      ((null? lst) res)
      ((pred (car lst)) (helper pred (cdr lst) (cons (car lst) res)))
      (else (helper pred (cdr lst) res))))
  (reverse (helper pred l '())))

; accumulate for range list instead of parameteres start / end
(define (accumulate l term comb init)
  (define (helper curr-l res)
    (if (null? curr-l)
        res
        (helper (cdr curr-l)
                (comb res (term (car curr-l))))))
  (helper l init))

; even squares sum
(define (even-sqr-sum l)
  (accumulate (filter (lambda (x) (= (remainder x 2) 0)) l) (lambda (x) (* x x)) + 0))

; apply procedure
(define (sum l)
  (apply + l))

; sum of sublists product
(define (sum-of-products l)
  (apply + (map (lambda (sublist) (apply * sublist))
                l)))

;min element in matrix
(define (min-matrix m)
  (apply min (map (lambda (l) (apply min l))
                  m)))

(define (min-matrix1 m)
  (map (lambda (l) (apply min l))
       m))

(define (get-nth-column m n)
  (map (lambda (row)
         (list-ref row n))
       m))

(define (filter-matrix p m)
  (map (lambda (row) (filter p row)) m))

(define (transpose m)
  (apply map list m))

(define (get-nth-column m n)
  (map (lambda (row) (list-ref row n)) m))

(define (transpose-2 m)
  (define (helper i)
    (if (= i (length m))
        '()
        (cons (get-nth-column m i)
              (helper (+ i 1)))))
  (helper 0))