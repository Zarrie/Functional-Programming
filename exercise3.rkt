;                                             -----------Exercise 3----------

;                                ---Task 1--- returns the reverse of given number's digits
(define (my-reverse number)
  (define (helper current result)
  (if (<= current 0)
      result
      (helper (quotient current 10)
                  (+ (* 10 result) (remainder current 10)))))
  (helper number 0))

;                                ---Task 2--- test if given number pattern occurs in number

(define (substr number pattern)
  (define (count-digits num)
    (if (= num 0)
        0
        (+ 1 (count-digits (quotient num 10)))))
  (define (helper number pattern)
    (if (= number 0)
        #f
        (if (= (remainder number (expt 10 (count-digits pattern))) pattern)
            #t
            (helper (quotient number 10) pattern))))
    (helper number pattern))

;                                ---Task 3--- computes the sqrt of given number

(define (my-sqrt x)
  (define epsilon 0.000001)
  (define (sqr-err tn)
    (abs (-
          (* tn tn)
           x)))
  (define (next tn)
    (/ (+ tn (/ x tn))
       2))
  (define (helper tn)
    (if (< (sqr-err tn) epsilon)
        tn
        (helper (next tn))))
  (helper 1))

;                                ---Task 3--- computes e^x by using Taylor series

(define (ex x)
  (define iterations 50)
  (define (fact num)
    (if (<= num 1)
        1
        (* num (fact (- num 1)))))
  (define (term n)
    (/ (expt x n)
       (fact n)))
  (define (helper iter result)
    (if (> iter iterations)
        result
        (helper (+ 1 iter)
                (+ result (term iter)))))
  (helper 0 0))