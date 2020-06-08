;Message Passing Style- Refinements
(load "error.scm")

(define (first lst)
  (list-ref lst 0))

(define (my-cons x y)
  (define (change-car new-car) (set! x new-car))
  (define (change-cdr new-cdr) (set! y new-cdr))
  (lambda (msg . args)
    (cond ((eq? msg 'CAR) x)
          ((eq? msg 'CDR) y)
          ((eq? msg 'PAIR?) #t)
          ((eq? msg 'SET-CAR!)
           (change-car (first args)))
          ((eq? msg 'SET-CDR!)
           (change-cdr (first args)))
          (else (error "pair cannot" msg)))))

(define (my-car p)
  (p 'CAR))

(define (my-set-car! p val)
  (p 'SET-CAR! val))
