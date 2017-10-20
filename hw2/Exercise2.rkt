;                                             -----------Exercise 2----------

;                                ---Task 1--- test if given number is prime
(define (prime? n)
  (helper (- n 1) n))

(define (helper possible-divisor n)
  (cond
    ((<= possible-divisor 1) #t)
    ((= (remainder n possible-divisor) 0) #f)
    (else (helper (- possible-divisor 1) n))))

;                                ---Task 2--- count number of digits in given number
(define (count-digits n)
  (if (= n 0)
      0
      (+ (count-digits (quotient n 10)) 1)))

; ------------------------------------- TAIL RECURSION VERSION TASK 2
(define (count-digits-t n)
  (count-digits-aux n 0))

(define (count-digits-aux n dig-num)
  (if (= n 0)
      dig-num
      (count-digits-aux (quotient n 10) (+ dig-num 1))))
; -------------------------------------

;                                ---Task 3--- sums the digits in given number
(define (sum-digits n)
  (if (<= n 9)
      (remainder n 10)
      (+ (sum-digits (quotient n 10)) (remainder n 10))))

; ------------------------------------- TAIL RECURSION VERSION TASK 3

(define (sum-digits-t n)
  (sum-digits-aux n 0))

(define (sum-digits-aux n sum)
  (if (<= n 9)
      (+ sum (remainder n 10))
      (sum-digits-aux (quotient n 10) (+ sum (remainder n 10)))))

; -------------------------------------

;                                ---Task 4--- convert given binary number into decimal 
(define (bin-to-dec n)
  (2-pow-pos n 0))

;                                auxiliary function for task 4
(define (2-pow-pos n pos)
  (if (> n 0) (if (= (remainder n 10) 1)
              (+ (expt 2 pos) (2-pow-pos (quotient n 10) (+ pos 1)))
              (+ 0 (2-pow-pos (quotient n 10) (+ pos 1))))
      0))

; ------------------------------------- TAIL RECURSION VERSION TASK 4

(define (bin-to-dec-t n)
  (2-pow-pos-aux n 0 0))

(define (2-pow-pos-aux n pos res)
  (if (> n 0)
      (if (= (remainder n 10) 1)
          (2-pow-pos-aux (quotient n 10) (+ pos 1) (+ res (expt 2 pos)))
          (2-pow-pos-aux (quotient n 10) (+ pos 1) res))
  res))
; -------------------------------------
          

;                                ---Task 5--- convert given decimal number into binary             
(define (dec-to-bin n)
  (10-pow-pos n 0))

;                                auxiliary function for task 5
(define (10-pow-pos n pos)
  (if (> n 0) (if (= (remainder n 2) 1)
                  (+ (expt 10 pos) (10-pow-pos (quotient n 2) (+ pos 1)))
                  (+ 0 (10-pow-pos (quotient n 2) (+ pos 1))))
      0))

; ------------------------------------- TAIL RECURSION VERSION TASK 5

(define (dec-to-bin-t n)
  (10-pow-pos-aux n 0 0))

(define (10-pow-pos-aux n pos res)
  (if (> n 0)
      (if (= (remainder n 2) 1)
          (10-pow-pos-aux (quotient n 2) (+ pos 1) (+ res (expt 10 pos)))
          (10-pow-pos-aux (quotient n 2) (+ pos 1) res))
      res))

; -------------------------------------

;                                ---Task 6--- test if given number is automorphic
(define (automorphic? n)
  (if (= (remainder (* n n) (expt 10 (count-digits n))) n)
      #t
      #f))
;                                auxiliary function for task 6
(define (cmp-first-last n pos-first pos-last)
  (if (= (quotient n (expt 10 pos-first)) (remainder n (expt 10 pos-last)))
      #t
      #f))


;                                ---Task 7--- test if given number is palindrome
(define (palindrome? n)
  (iter n 0 (count-digits n)))

(define (palindrome? n)
  (eq-f-l n))

;                                auxiliary function for task 7
(define (eq-f-l n)
  (if (= n 0)
      #t
      (if (= (quotient n (expt 10 (- (count-digits n) 1))) (remainder n 10))
          (eq-f-l (quotient (remainder n (expt 10 (- (count-digits n) 1))) 10))
          #f)))
