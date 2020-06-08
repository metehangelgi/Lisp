


(define (test x n)
  (cond ((= n 0) 1)
        ((even? n) (sqr (test x (/ n 2))))
        (else test (* x (test x (- n 1))))))







(define deriv (lambda (f)
(lambda (x) (/ (- (f (+ x epsilon)) (f x)) epsilon))))

(define square (lambda (y) (* y y)))
(define epsilon 0.001)

((deriv square) 5)
(test 2 10)