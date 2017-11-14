(define (derive f)
  (lambda (x)
    (let
        ((x0 (+ x 0.0001)))
      (/ (- (f x) (f x0)) (- x x0)))))

(define (derive-x f)
  (lambda (x y)
    (let
        ((h 0.0001))
      (/ (- (f (+ x h) y) (f x y)) h))))

(define (derive-y f)
  (lambda (x y)
    (let
        ((h 0.0001))
      (/ (- (f x (+ y h)) (f x y)) h))))

(define (accumulate start next end term comb initial)
  (define (helper current result)
    (if (> current end)
        result
        (helper (next current)
                (comb result (term current)))))
  (helper start initial))

(define (compose f g)
  (lambda (x)
    (g (f x))))

(define (repeat f n)
  (accumulate 1 (lambda (n) (+ n 1)) n (lambda (_) f) (lambda (f g) (compose f g)) (lambda (_) (lambda (x) x))))
