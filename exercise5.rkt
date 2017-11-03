(define (accumulate start end step term comb initial)
  (define (helper curr result)
    (if (> curr end)
        result
        (helper (step curr)
                (comb result (term curr)))))
  (helper start initial))

(define (fact n)
  (accumulate 1 n (lambda (x) (+ x 1)) (lambda (x) x) * 1))

(define (pow x n)
  (accumulate 1 n (lambda (k) (+ k 1)) (lambda (_) x) * 1))

(define (variation k n)
  (/ (accumulate 1 n (lambda (x) (+ x 1)) (lambda (x) x) * 1)
     (accumulate 1 (- n k) (lambda (x) (+ x 1)) (lambda (x) x) * 1)))

(define (combination k n)
  (/ (variation k n) (accumulate 1 k (lambda (x) (+ x 1)) (lambda (x) x) * 1)))

(define (ex x)
  (define precision 100)
  (accumulate 0
              precision
              (lambda (n)
                (+ n 1))
              (lambda (i)
                (/ (pow x i)
                   (fact i)))
              + 0))
(define (my-sin x)
  (define precision 100)
  (accumulate 0
              precision
              (lambda (n)
                (+ n 1))
              (lambda (n)
                (/ (* (pow -1 n)
                      (pow x (+ (* 2 n) 1)))
                   (fact (+ (* 2 n) 1))))
              + 0))

(define (my-sqrt x)
  (define precision 20)
  (define (next-tn tn _)
    (/ (+ tn (/ x tn)) 2))
  (accumulate 1
              precision
              (lambda (n)
                (+ n 1))
              (lambda (y)
                y)
              next-tn
              1))
                   
  