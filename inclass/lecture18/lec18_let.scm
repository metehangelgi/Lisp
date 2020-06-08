;; Let Syntax Transformation

(load "lec18_1.scm")

(define (let? exp) (tagged-list? exp 'let))
(define (let-bound-variables let-exp) (map car (cadr let-exp)))
(define (let-values let-exp) (map cadr (cadr let-exp)))
(define (let-body let-exp) (sequence->exp (cddr let-exp)))

(define (let->combination let-exp)
  (let ((names (let-bound-variables let-exp))
        (values (let-values let-exp))
        (body (let-body let-exp))) (cons (list 'lambda names body)
                                         values)))