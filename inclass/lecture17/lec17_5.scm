;; 5. Environment as explicit parameter

(load "lec17_4.scm")

; extend eval to take an expression and
; an environment as arguments
; => evaluate an expression by specifying whict environment
(define (my-eval exp env)
  (cond
    ; dispatch functions also take env as arg
    ((number? exp) exp)
    ((symbol? exp) (lookup exp env))
    ((define? exp) (eval-define exp env))
    ((if? exp)     (eval-if exp env))
    ((application? exp) (my-apply (my-eval (car exp) env)
                                  (map (lambda (e) (my-eval e env))
                                       (cdr exp))))
    (else
     (error "unknown expression " exp))))

(define (lookup name env)
(let ((binding (table-get env name)))
    (if (eq? binding #f)
        (error "unbound variable: " name)
        (binding-value binding))))

(define (eval-define exp env)
  (let ((name (cadr exp))
        (defined-to-be (caddr exp)))
    (table-put! env name (my-eval defined-to-be env))
    'undefined))

(define (eval-if exp env)
  (let ((predicate (cadr exp))
        (consequent  (caddr exp))
        (alternative (cadddr exp)))
    (let ((test (my-eval predicate env)))
      (cond
        ((eq? test #t) (my-eval consequent env))
        ((eq? test #f) (my-eval alternative env))
        (else          (error "val not boolean: "
                              predicate))))))

(my-eval '(define* z* (plus* 4 5)) environment)
(my-eval '(if* (greater* z* 6) 10 15) environment)
