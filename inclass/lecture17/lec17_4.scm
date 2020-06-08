;; 4. Store operators in the environment

(load "lec17_3.scm")

(define (application? e) (pair? e))

(define (my-eval exp)
  (cond
    ((number? exp) exp)
    ((symbol? exp) (lookup exp))
    ((define? exp) (eval-define exp))
    ((if? exp)     (eval-if exp))
    ((application? exp) (my-apply (my-eval (car exp))
                                  (map my-eval (cdr exp))))
    (else
     (error "unknown expression " exp))))

(define (my-apply operator operands)
  (if (primitive? operator)
      (apply (get-scheme-procedure operator) operands)
      (error "operator not a procedure: " operator)))

;; primitive: an ADT that stores scheme procedures
(define prim-tag 'primitive)
(define (make-primitive scheme-proc) (list prim-tag scheme-proc))
(define (primitive? e) (tag-check e prim-tag))
(define (get-scheme-procedure prim) (cadr prim))

(define environment (make-table))
(table-put! environment 'plus* (make-primitive +))
(table-put! environment 'greater* (make-primitive >))
(table-put! environment 'true* #t)

(my-eval '(define* z* 9))
(my-eval '(plus* 9 6))
(my-eval '(if* true* 10 15))
