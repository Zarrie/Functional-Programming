(define (nth lst n)
  (if (null? lst)
      (error "Out of list range")
      (if (= n 1)
          (car lst)
          (nth (cdr lst) (- n 1)))))

(define (concat l1 l2)
  (cond
    ((not (null? l1)) (cons (car l1) (concat (cdr l1) l2)))
    ((not (null? l2)) (cons (car l2) (concat l1 (cdr l2))))
    (else '())))

(define (len lst)
  (define (atom? x)
  (and (not (null? x))
       (not (pair? x))))
  (if (null? lst)
      0
      (if (atom? (car lst))
          (+ 1 (len (cdr lst)))
          (+ (len (car lst)) (len (cdr lst))))))

(define (rev lst)
  (define (rev-help lst new-lst)
    (if (null? lst)
        new-lst
        (rev-help (cdr lst) (cons (car lst) new-lst))))
  (rev-help lst '()))

(define (count-atoms lst)
  (define (atom? x)
  (and (not (null? x))
       (not (pair? x))))
  (if (null? lst)
      0
      (if (atom? (car lst))
          (+ 1 (count-atoms (cdr lst)))
          (count-atoms (cdr lst)))))





