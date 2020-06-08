;; 1. Arithmetic calculator

(load "error.scm")

(define (tag-check e sym) (and (pair? e) (eq? (car e) sym)))

(define (sum? e) (tag-check e '+))

(define (my-eval exp)
  (cond
   ((number? exp) exp)
   ((sum? exp)    (eval-sum exp))
   (else
    (error "unknown expression " exp))))

(define (eval-sum exp)
   (+ (my-eval (cadr exp)) (my-eval (caddr exp))))

(my-eval '(+ 24 (+ 5 6)))     