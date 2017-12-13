(define (make-queue)
    (cons '() '()))

(define (empty? queue)
  (null? (car queue)))

(define (front queue)
  (if (empty? queue)
      queue
      (caar queue)))

(define (push! queue element)
  (let ((newelement (cons element '())))
  (if (empty? queue)
      (begin
        (set-car! queue newelement)
        (set-cdr! queue newelement)
        (car queue))
      (begin
        (set-cdr! (cdr queue) newelement)
        (set-cdr! queue newelement)
        (car queue)))))

(define (pop! queue)
  (if (empty? queue)
      (error "Empty queue deletion!")
      (begin (set-car! queue (cdr (car queue)))
             (car queue))))

(define q (make-queue))

(empty? q)

(push! q 5)
(push! q 10)
(push! q 20)

(empty? q)

(front q)

(pop! q)
(pop! q)
(pop! q)

(empty? q)
