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
