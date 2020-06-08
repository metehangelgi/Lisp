#lang racket

(define improve
    (lambda (guess x)
        (average guess (/ x guess))))


(define sqrt-loop (lambda (G X)
   (if (close-enuf? G X)
       G
       (sqrt-loop (improve G X) X))))

(define close-enuf? 
     (lambda (guess x)
        (< (abs (- (square guess) x)) 0.000000001)))

(define square (lambda (x) (* x x)))

(define mysqrt 
    (lambda (x)
        (sqrt-loop 1.0 x)))

(define average 
    (lambda (a b) (/ (+ a b) 2)))


(- (mysqrt 2) (sqrt 2))


