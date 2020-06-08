(define quadratic-root
  (lambda (a b c)
    ( / (+ (- 0 b)  (- (* b b) (* 4 (* a c)))) (* 2 a ))))



(quadratic-root 1 5 6)



(define pythagoras (lambda (a b) 
( sqrt (+ (* a a) (* b b)))))



(pythagoras 3 4)


(define bigger2 (lambda (a b)
                  (cond (( > a b) a)
                        (else b))))

(bigger2 -23 4)


(define bigger3 (lambda (a b c)
                  (cond ((< (bigger2 a b) (bigger2 b c)) bigger2 b c)
                        (else (bigger2 a b)))))


(bigger3 1 24 -3)


(define second-order (lambda (x)
                       ( - ( + (* 3 (* x x)) (* 14 x)) 5)))

(second-order 5)






(define quadratic-root
  (lambda (a b c)
    ( if ( < (+ (- 0 b)  (- (* b b) (* 4 (* a c)))) (- (- 0 b)  (- (* b b) (* 4 (* a c)))))
         (/ (- (- 0 b)  (- (* b b) (* 4 (* a c)))) (* 2 a))
         (/ (+ (- 0 b)  (- (* b b) (* 4 (* a c)))) (* 2 a)))))



 (quadratic-root 1 1 6)


