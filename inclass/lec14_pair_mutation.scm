;Pair Mutation as Change in State
(load "error.scm")

(define (my-cons x y)
  (lambda (msg)
    (cond ((eq? msg 'CAR) x)
          ((eq? msg 'CDR) y)
          ((eq? msg 'PAIR?) #t)
          ((eq? msg 'SET-CAR!)
           (lambda (new-car) (set! x new-car)))
          ((eq? msg 'SET-CDR!)
           (lambda (new-cdr) (set! y new-cdr)))
          (else (error "pair cannot" msg)))))

(define (my-car p)
  (p 'CAR))

(define (my-cdr p)
 (p 'CDR))

(define (my-pair? p)
  (and (procedure? p) (p 'PAIR?)))

(define (my-set-car! p new-car)
  ((p 'SET-CAR!) new-car))

(define (my-set-cdr! p new-cdr)
  ((p 'SET-CDR!) new-cdr))

(define bar (my-cons 3 4))
(my-set-car! bar 0)
(my-car bar)

