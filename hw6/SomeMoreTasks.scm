(define (flatten l)
  (define (atom? el)
    (not (pair? el)))
  (define (helper result lst)
    (cond
      ((null? lst) result)
      ((atom? (car l)) (cons (car l) (flatten (cdr l))))
      (else (append (flatten (car l)) (flatten (cdr l))))))
  (helper '() l))

(define (sum lst)
  (define (helper lst res)
    (if (null? lst)
        res
        (helper (cdr lst) (+ (car lst) res))))
  (helper (flatten lst) 0))

(define (prod lst)
  (define (helper lst res)
    (if (null? lst)
        res
        (helper (cdr lst) (* (car lst) res))))
  (helper (flatten lst) 1))

(define (accumulate start next end term comb initial)
  (define (helper current result)
    (if (> current end)
        result
        (helper (next current)
                (comb result (term current)))))
  (helper start initial))

(define (fact n)
  (accumulate 1 (lambda (x) (+ x 1)) n (lambda (x) x) * 1))

(define (fn f n)
  (define (p+1 x) (+ 1 x))
  (define (compose f g)
    (lambda (x) (f (g x))))
  (define (identity x) x)
  (accumulate 1 p+1 n (lambda (_) f) compose identity))

;(flatten '(1 2 (3) 4 (5 (6))))
;(sum '(1 2 (3) 4 (5 (6))))
;(prod '(1 2 (3) 4 (5 (6))))
;((fn (lambda (x) (* 2 x)) 2000) 2)
