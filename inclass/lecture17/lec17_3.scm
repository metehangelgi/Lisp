;; 3. Conditionals and if

(load "lec17_2.scm")

(define (greater? exp) (tag-check exp 'greater*))
(define (if? exp)      (tag-check exp 'if*))

(define (my-eval exp)
  (cond
    ((number? exp) exp)
    ((sum? exp) (eval-sum exp))
    ((symbol? exp) (lookup exp))
    ((define? exp) (eval-define exp))
    ((greater? exp) (eval-greater exp))
    ((if? exp) (eval-if exp))
    (else
     (error "unknown expression " exp))))
  
(define (eval-greater exp)
  (> (my-eval (cadr exp)) (my-eval (caddr exp))))

(define (eval-if exp)
  (let ((predicate   (cadr exp))
        (consequent  (caddr exp))
        (alternative (cadddr exp)))
    (let ((test (my-eval predicate)))
      (cond
        ((eq? test #t) (my-eval consequent))
        ((eq? test #f) (my-eval alternative))
        (else          (error "predicate not a conditional: "
                              predicate))))))

(my-eval '(define* y* 9))
(my-eval '(if* (greater* y* 6) (plus* y* 2) 15))


