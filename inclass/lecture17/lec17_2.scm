;; 2. Names

(load "lec17_1.scm")
(load "lec17_table.scm")

(define (define? exp) (tag-check exp 'define*))

(define (my-eval exp)
  (cond
    ((number? exp) exp)
    ((sum? exp) (eval-sum exp))
    ((symbol? exp) (lookup exp))
    ((define? exp) (eval-define exp))
    (else
     (error "unknown expression " exp))))

(define environment (make-table))

(define (lookup name)
  (let ((binding (table-get environment name)))
    (if (null? binding)
        (error "unbound variable: " name)
        (binding-value binding))))

(define (eval-define exp)
  (let ((name (cadr exp))
        (defined-to-be (caddr exp)))
    (table-put! environment name (my-eval defined-to-be))
    'undefined))

(my-eval '(define* x* (plus* 4 5)))
(my-eval '(plus* x* 2))