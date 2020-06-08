(#%require racket)

(define (my-cons x y)
  (lambda (msg)
    (cond ((eq? msg 'CAR) x)
          ((eq? msg 'CDR) y)
          ((eq? msg 'PAIR?) #t)
          (else (error "pair cannot" msg)))))

(define (my-car p)
  (p 'CAR))

(define (my-cdr p)
 (p 'CDR))

(define (my-pair? p)
  (and (procedure? p) (p 'PAIR?)))

(define foo (my-cons 1 2))
(my-car foo) ;becomes (foo 'CAR)

