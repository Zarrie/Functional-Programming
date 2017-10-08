(define fact (lambda (x)
              (if (<= x 1)
                  1
                  (* x (fact (- x 1))))))

(define digits-list (lambda (x)
                      (if (<= x 0)
                          '()
                          (cons (remainder x 10) (digits-list (quotient x 10))))))

(define (digits n)
  (if (<= n 0)
      '()
      (cons (remainder n 10) (digits (quotient n 10)))))

(define (list-len lst)
  (if (null? lst)
      0
      (if (equal? (cdr lst) '())
          1
          (+ (list-len (cdr lst)) 1))))

(define rev (lambda (lst)
              (if (= (list-len lst) 1) (car lst)
              (list (rev (cdr lst)) (car lst) ))))
                     
(define (merge l1 l2)
      (if (null? l1) l2
          (if (null? l2) l1
              (cons (car l1) (cons (car l2) (merge (cdr l1) (cdr l2)))))))