;                                             -----------Exercise 1----------

;                                ---Task 1--- sums two given numbers
(define sum (lambda (a b)
              (+ a b)))

;                                ---Task 2--- checks if given number is odd
(define odd (lambda (x)
              (= (remainder x 2) 1)))

;                                ---Task 3--- checks if given number is even
(define even (lambda (x)
              (= (remainder x 2) 0)))

;                                ---Task 4--- checks the sign of given number
(define sign (lambda (x)
               (cond
                 ((< x 0) -1)
                 ((> x 0) 1)
                 ((= x 0) 0))))

;                                ---Task 5--- returns the factorial of given number
(define factorial (lambda (n)
                    (if (< n 2)
                        1
                        (* n (factorial(- n 1))))))

(define factorial-2 (lambda (n)               ;Same factorial function with cond instead of if
                    (cond
                      ((<= n 1) 1)
                      (else (factorial (- n 1))))))

;                                ---Task 6--- sums all the numbers in given interval - start/end
(define sum-interval (lambda (start end)
                       (if (= start end)
                           end
                           (+ start (sum-interval (+ start 1) end)))))

;                                ---Task 7--- power function
(define pow (lambda (x n)
              (if (= n 0)
                  1
                  (* x (pow x (- n 1))))))

;                                ---Task 8--- fast power algorithm implementation
(define fast-pow (lambda (x n)
                   (if (even n) (* (pow x (/ n 2)) (pow x (/ n 2)))
                       (* x (pow x (- n 1))))))
