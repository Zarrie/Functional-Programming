; Returns n-th column in given matrix m
(define (nth m n)
  (map (lambda (row) (list-ref row n)) m))

; Returns tranposed of given matrix m
(define (transpose m)
  (define (helper i)
    (if (= i (length m))
        '()
        (cons (nth m i)
              (helper (+ i 1)))))
  (helper 0))

; Magical transpose
(define (transpose-2 m)
  (apply map list m))

; Returns list of elements from given list composed of lists
(define (flatten l)
  (define (atom? el)
    (not (pair? el)))
  (define (helper result lst)
    (cond
      ((null? lst) result)
      ((atom? (car l)) (cons (car l) (flatten (cdr l))))
      (else (append (flatten (car l)) (flatten (cdr l))))))
  (helper '() l))

