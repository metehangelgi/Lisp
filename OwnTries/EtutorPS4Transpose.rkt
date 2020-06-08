
(#%require racket)
(define nil empty) 

(define make-increasing-matrix
  (lambda (rows cols start)
    (if (= rows 0) nil
        (cons (make-increasing-list cols start) (make-increasing-matrix (- rows 1) cols (+ start cols)))
        )))

(define (make-increasing-list cols start)
  (if (= cols 0) nil
      (cons start (make-increasing-list (- cols 1) (+ start 1)))))

(define make-column-increasing-matrix
  (lambda (rows cols start) 
    (transpose (make-increasing-matrix rows cols start))))

(make-increasing-matrix 3 5 10)
(make-increasing-matrix 5 5 10)

(define transpose (lambda (lst)
    (if (null? lst) empty                
  (if (empty? (car lst)) empty
     (cons (map car lst) (transpose (map cdr lst)))))))

(define transpose
  (lambda (xss)
    (cond
      ((empty? xss)         empty)
      ((empty? (first xss)) empty)
      (else                 (define first-column   (map first xss))
                            (define other-columns  (map rest  xss))
                            (cons first-column
                                  (transpose other-columns))))))
(define (transpose xss)
  (apply map list xss))


(transpose (make-increasing-matrix 3 5 10))
(transpose (make-increasing-matrix 5 5 10))
(transpose '((1 2 3) (4 5 6)))
(transpose '((1 2 3 4 5)))
(transpose '())
