'((a b) (b))
(list (list 'a 'b) (list 'b))

(let ((z '(a b))) (list z (list 'b)))

(let ((z '(a b))) (list z (cdr z)))

(let ((z (list (list 'a 'b)))) (set-cdr! z (cdar z)) z)

(let ((z (list (list 'a 'b)))) (set-cdr! z (list (cdar z))) z)



(define make-color
  (lambda (red-amt green-amt blue-amt)
    (list (restrict red-amt)
	  (restrict green-amt)
	  (restrict blue-amt))))

(define red-val car)
(define green-val cadr)
(define blue-val caddr)

(define restrict
  (lambda (x)
    (cond ((< x 0) 0)
	  ((> x 255) 255)
	  (else (round x)))))

(define half-as-bright
  (lambda (color)
    (map (lambda (val) (/ val 2)) color)))

(half-as-bright (make-color 100 150 200))
(half-as-bright (make-color 75 100 125))



(define last-cons
  (lambda (l)
    (if (null? (cdr l)) l
        (last-cons (cdr l)))))

(define nconc! (lambda (l1 l2)
    (set-cdr! (last-cons l1) l2)
    l1))

(define x (list 'a 'b))
(define y (list 'c 'd))
(nconc! x y)
x




(define fill-queue! (lambda (queue elts) 
                      (if (null? elts)
                          queue
                          (fill-queue! (insert-queue! queue (car elts)) (cdr elts) ))
                          )) 


(define *test* '(a b c a b a b a b c  b c b c a b c))

(define begins-with (lambda (starter-list full-list)
  (cond ((null? starter-list) #t)
        ((null? full-list) #f)
        ((eq? (car starter-list) (car full-list)) (begins-with (cdr starter-list) (cdr full-list)))
        (else #f)
  )))

(define match
  (lambda (pattern text)
    (define begins-with (lambda (starter-list full-list)
  (cond ((null? starter-list) #t)
        ((null? full-list) #f)
        ((or (eq? (car starter-list) (car full-list)) (equal? (car starter-list) '*)) (begins-with (cdr starter-list) (cdr full-list)))
        (else #f)
  )))
    (cond ((null? text) 0)
          ((begins-with pattern text) (+ 1 (match pattern (cdr text))))
          (else (match pattern (cdr text))))
    ))

(match '(a b c) *test*)
(match '(a b c) '())
(match '(a b c d) *test*)
(match '(a) *test*)
(match* '(a b c) *test*)
