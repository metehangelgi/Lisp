;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname work1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(define sqrMe   (lambda (x) (* x x)))


(sqr 5)


;((lambda (x)(* x x)) 5)

( sqr 5)

(define pythagoras (lambda (x y) (sqrt (+ (* x x)(* y y)))))


(pythagoras 3 4 )


(define square (lambda (x) (* x x)))

(define close-enuf? 
     (lambda (guess x)
        (< (abs (- (square guess) x)) 0.00000001)))

(define average 
    (lambda (a b) (/ (+ a b) 2)))


(define improve
    (lambda (guess x)
        (average guess (/ x guess))))


(define sqrt-loop (lambda (G X)
   (if (close-enuf? G X)
       G
       (sqrt-loop (improve G X) X))))

(define mysqrt 
    (lambda (x)
        (sqrt-loop 1.0 x)))

(- (mysqrt 2) (sqrt 2))

(mysqrt 4)



