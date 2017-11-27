(define (accumulate start next end term comb init)
  (if (> start end)
      init
      (comb (term start)
            (accumulate (next start) next end term comb init))))

(define (filter L pred)
  (cond
    ((null? L) '())
    ((pred (car L)) (cons (car L) (filter (cdr L) pred)))
    (else (filter (cdr L) pred))))

(define (map! L term)
  (if (null? L)
      '()
      (begin
        (set-car! L (term (car L)))
        (map! (cdr L) term)
        L)))

(define (my-map L term)
  (define (helper lst res)
    (if (null? lst)
        res
        (helper (cdr lst) (cons (term (car lst)) res))))
  (helper L '()))

(define (my-map-1 L term)
  (if (null? L)
      '()
      (cons (term (car L)) (my-map-1 (cdr L) term))))

(define (ld L)
  (define (ldp lst res)
    (cond
      ((null? lst) res)
      ((>= (car lst) (car res)) res)
      (else (ldp (cdr lst) (cons (car lst) res)))))
  (define (len l)
    (if (null? l)
        0
        (+ 1 (len (cdr l)))))
  (define (helper lst res)
    (if (null? lst)
        res
        (if (> (len (ldp (cdr lst) (cons (car lst) '()))) (len res))
            (helper (cdr lst) (ldp (cdr lst) (cons (car lst)'())))
            (helper (cdr lst) res))))
  (reverse (helper (cdr L) (ldp (cdr L) (cons (car L) '())))))

; for natural number n, precious(n) ::= #t if n's digits are
; in nonincreasing order, #f otherwise

(define (precious? n)
  (define (digits n_)
    (if (<= n_ 9)
        1
        (+ 1 (digits (quotient n_ 10)))))
  (cond
    ((< n 10) #t)
    ((>= (quotient n (expt 10 (- (digits n) 1)))
         (remainder (quotient n (expt 10 (- (digits n) 2))) 10))
     (precious? (remainder n (expt 10 (- (digits n) 1)))))
    (else #f)))
