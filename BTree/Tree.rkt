(define (makeTree leftSubTree root rightSubTree)
  (list leftSubTree root rightSubTree))

(define (subTree part tree)
  (if (null? tree)
      '()
      (part tree)))

(define (root tree)
  (subTree cadr tree))

(define (leftSubTree tree)
  (subTree car tree))

(define (rightSubTree tree)
  (subTree caddr tree))

(define (leaf? tree)
  (and (null? (leftSubTree tree))
       (null? (rightSubTree tree))))

(define tree (makeTree (makeTree '() 1 '()) 2 (makeTree '() 3 '()))) 

(define (member? tree element)
  (if (null? tree)
      #f
      (or (= (root tree) element)
          (member? (leftSubTree tree) element)
          (member? (rightSubTree tree) element))))

(define (insert! tree element)
  (if (null? tree)
      (set! tree (makeTree '() element '()))
      (if (leaf? tree)
          (cond
            ((> (root tree) element) (begin
                                       (set! tree (makeTree (makeTree '() element '()) (root tree) '()))
                                       tree))
            ((< (root tree) element) (begin
                                       (set! tree (makeTree '() (root tree) (makeTree '() element '())))
                                       tree))
            ((= (root tree) element) '()))
          (cond
            ((= (root tree) element) '())
            ((> (root tree) element) (insert! (leftSubTree tree) element))
            (else  (insert! (rightSubTree tree) element))))))
