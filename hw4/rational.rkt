(define (make-rat nom denom)
  (define (gcd first second)
    (cond
      ((= first second) first)
      ((> first second) (gcd (- first second) second))
      (else (gcd first (- second first)))))
  (if (= nom 0) (cons 0 1)
      (let ((gcd-curr (gcd (abs nom) (abs denom))))
        (if (or (and (positive? nom)
                     (positive? denom))
                (and (negative? nom)
                     (negative? denom)))
            (cons (/ (abs nom) gcd-curr)
                  (/ (abs denom) gcd-curr))
            (cons (- (/ (abs nom) gcd-curr))
                  (/ (abs denom) gcd-curr))))))

(define (nom rat)
  (car rat))

(define (denom rat)
  (cdr rat))

(define (+-rat r1 r2 operation)
  (make-rat (operation (* (nom r1)
                               (denom r2))
                            (* (denom r1)
                               (nom r2)))
                 (* (denom r1) (denom r2))))

(define (+rat r1 r2)
  (+-rat r1 r2 +))

(define (-rat r1 r2)
  (+-rat r1 r2 -))

(define (*rat r1 r2)
  (make-rat
   (* (nom r1)
      (nom r2))
   (* (denom r1)
      (denom r2))))

(define (/rat r1 r2)
  (make-rat
   (* (nom r1)
      (denom r2))
   (* (denom r1)
      (nom r2))))

(define (=rat? r1 r2)
  (= (* (nom r1)
        (denom r2))
     (* (denom r1)
        (nom r2))))

(define (print-rat r)
  (cond
    ((= (nom r) 0) (display (nom r)))
    ((= (denom r) 1) (display (nom r)))
    (else
     (display (nom r))
     (display "/")
     (display (denom r)))))
    
  
