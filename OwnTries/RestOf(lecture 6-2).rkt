(define (fixed-point f i-guess)
  (define (try g)
    (if (close? (f g) g)
        (f g)
        (try (f g))))
  (try i-guess))

(define (close? a b)
  (< (abs (- a b)) 0.0001))


(define (my-sqrt x)
  (fixed-point
   (lambda (y) (/ x y)) 1))

;;(my-sqrt 2)--infinite loop

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define average (lambda (a b)
                  (/ (+ a b) 2)))


(define square (lambda (x)
  (* x x)))

((average-damp square) 10)




;sqrt with damp-average
(define (sqrtt x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x y))) 1))

(sqrtt 16)


;chcck ---Heron of Alexandria's algorithm for square roots
;cubic root
(define (cbrt x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x (square y)))) 1))

;(cbrt 27)- couldn't finish ?

(define hop1 (lambda (f x) (+ 2 (f (+ x 1)))))



(define compose (lambda (f g x) (f (g x))))

; (A -> B),(C -> A),C -> B


