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