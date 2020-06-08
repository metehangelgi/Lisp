
(define accumulate-interval (lambda (op init lower upper)
  (if (> lower upper)
      init
      (op lower
          (accumulate-interval op init (+ 1 lower) upper)))))


(define count-true (lambda (pred lower upper)
  (cond ((> lower upper) 0)
        ((pred lower) (+ 1 (count-true pred (+ 1 lower) upper)))
        (else (count-true pred (+ 1 lower) upper)))))

(define count-true (lambda (pred lower upper)
  (define (helper pred lower upper count)
    (cond ((> lower upper) count)
          ((pred lower) (helper pred (+ 1 lower) upper (+ 1 count)))
          (else (helper pred (+ 1 lower) upper count))))
  (helper pred lower upper 0)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define f (lambda (m b)
                (lambda (x) (+ (* m x) b))))
(define g (f 3 2))


(define repeatedly-apply (lambda (p n)
  (if (= n 1) p
      (compose p (repeatedly-apply p (- n 1))))))


((repeatedly-apply (lambda (x) (+ 1 x)) 10) 2)


(define curry (lambda (p)
  (lambda (x) (lambda (y) (p x y)))))