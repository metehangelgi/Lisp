;;Derivative
(define deriv (lambda (f)
                (lambda (x)
                  (/ (- (f (+ x epsilon)) (f x))
                     epsilon))))

(define square
  (lambda (y) (* y y)))

(define epsilon 0.001)

((deriv square) 5)
;;Integration

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b n)
  (let ((delta (/ (- b a) n)))
    (* (sum f a (lambda (x)(+ x delta)) b) delta)))

(define my-atan
  (lambda (a)
    (integral (lambda (x) (/ 1 (+ 1 (square x)))) 0 a 1000)))

(my-atan 2.0)
(my-atan 3.0)
