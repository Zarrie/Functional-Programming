;----------------------------------------------------------------------------------------------------------------------------------

;                                             -----------Assignement 1 solutions----------

;                                ---Task 1---
;convering temperature F -> C                                                                       // task1
(define (f-to-c x)
  (* (/ 5 9) (- x 32)))

;                                ---Task 2---
;function returning true if exactly one number in given list is == 2 else returns false             // task2
(define (three-grades x y z)
                        (if (= (count (list x y z) 2) 1)
                            #t
                            #f))

;auxiliary function for task 2 which counts number occurances of el in lst
(define (count lst el)                                 ;Taking as argument list and element
  (cond ((null? lst) 0)                                ;If list is empty return 0
        ((= (car lst) el) (+ 1 (count (cdr lst) el)))  ;If list's first element = el return (count(crd list)) + 1
        (else (count (cdr lst) el))))                  ;If list's first element != el countinue with crd list