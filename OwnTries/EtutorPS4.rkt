
(define make-increasing-matrix
  (lambda (rows cols start)
    (make-increasing-list rows cols start (list))
    ))



;(define (make-increasing-list rows cols start lst)
 
 ;)

;(make-increasing-matrix 3 4 10)








(#%require racket)
(define nil empty) 

(cons 1 (cons 2 (cons 3 nil)))



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

(define transpose (lambda (m) your_code_here))

(make-increasing-matrix 3 5 10)
(make-increasing-matrix 5 5 10)

(define (contains? list elt)
  (cond ((null? list) #f)
        ((equal? elt (car list)) #t)
        (else (contains? (cdr list) elt))))









(define subset?
  (lambda (lst1 lst2)
    (cond ((null? lst1) #t)
          ((contains? lst2 (car lst1)) (subset? (cdr lst1) lst2))
          (else #f))))


(subset? nil (list 1 2 3))
(subset? (list 2 3) (list 1 2 3))

(subset? (list 2 3 4) (list 1 2 3))

(cons 6 nil)


(define thing (cons (cons 1 (cons 2 nil)) (cons (cons 3 4) nil)))

(cdr thing)


(define thing (cons (cons 'a nil) (cons 'b (cons 'c nil))))

thing


(define contains?
  (lambda (lst elt)
    (cond ((null? lst) #f)
          ((equal? (car lst) elt) #t)
          (else (contains? (cdr lst) elt)))))


(contains? (list 1 2 3) 3)
(contains? (list 1 2 3) 4)
(contains? nil 3)




(define *infinity* 99999999999)
            
(define min-list (lambda (lst)
                   (cond ((null? lst) *infinity*)
                   (else (min (car lst) (min-list (cdr lst)))))))


(min-list '())
(min-list '()) ;=> 999999999999  *infinity*
  (min-list (list 1 2 3)) ;=> 1
  (min-list (list -100 -1000 -10000)) ;=> -10000
  (min-list (list 1 1 1 1 1 1)) ;=> 1

(define divisible? (lambda (x y)
  (if (= (remainder x y) 0) #t #f)))



(define (divisible? b a)
  (if (zero? a) #f (zero? (remainder b a))))

(define (remove-divisible list a)
  (filter (lambda (x) (not (divisible? x a))) list))

(remove-divisible (list 1 2 3 4 5 6) 3)

(define sieve
  (lambda (lst)
    (if (null? lst)
      nil
      (cons (car lst)
            (sieve (filter
                     (lambda (x) (not (divisible? x (car lst))))
                     (cdr lst)))))))

(define (sieve list)
  (if (null? list) nil
      (cons (car list) (sieve (remove-divisible (cdr list) (car list))))))


(sieve (list 1 2 3 4 1 2))
(sieve (list 2 3 4 5 7 1 6))


(define (sieve list)
  (cond ((null? list) empty)
        ((contains? (cdr list) (car list)) (sieve (cdr list)))
        (else (cons (car list) (sieve (cdr list))))))


(sieve (list 1 2 3 4 1 2))


(define (generate-interval a b)
  (cond ((< b a) nil)
        ((= a b) nil)
        (else (cons a (generate-interval (+ a 1) b)))))

(generate-interval 3 5)


(define flatten-tree
  (lambda (tree)
    (cond ((null? tree) nil)
          ((null? (car tree)) (flatten-tree (cdr tree)))
          ((number? (car tree)) (cons (car tree) (flatten-tree (cdr tree))))
          (else (cons (flatten-tree (car tree)) (flatten-tree (cdr tree)))))
          ))


(flatten-tree '(1 2 (3) (2 3 4) (((5)))))



(number? (car (car (car (caddr '((1) (2 3 4) (((5)))))))))


(define test   '((1) (2 3 4) (((5)))))

(cons (car (car test)) (cdr test))


(define (reduce op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (reduce op init (cdr lst)))))

(define fold-right reduce)

(define (leaf? x)
  (not (pair? x)))

(define flatten-tree
  (lambda (tree)
    (fold-right cons nil tree)
    ))

(flatten-tree '(1 2 (3) (2 3 4) (((5)))))


(define fold-right-tree
  (lambda (op id tree)
    ))


  (fold-right-tree + 0  tree)