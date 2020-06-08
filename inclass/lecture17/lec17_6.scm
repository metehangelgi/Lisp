;; 6. Defining new procedures

(load "lec17_5.scm")

; a new tag checker for lambda* expressions
(define (lambda? e) (tag-check e 'lambda*))

(define (my-eval exp env)
  (cond
    ((number? exp) exp)
    ((symbol? exp) (lookup exp env))
    ((define? exp) (eval-define exp env))
    ((if? exp)     (eval-if exp env))
    ; a new dispatch to handle lambda* objects
    ; order! before application, after primitives
    ((lambda? exp) (eval-lambda exp env))
    ((application? exp) (my-apply (my-eval (car exp) env)
                                  (map (lambda (e) (my-eval e env))
                                       (cdr exp))))
    (else
     (error "unknown expression " exp))))

; in: a tree structure representing a lambda* exp
;     a pointer to an env
(define (eval-lambda exp env)
  ; walk down the tree
  ; pull out pieces without evaluating (no call to eval)
  (let ((args (cadr exp))    ; formal parameters
        (body (caddr exp)))  ; body of the expression (exactly one exp)
    ; glue them together
    ; => a procedure as a set of args, a body, and an env
    ; like the "double bubble" in EM
    (make-compound args body env)))

; eval the first sub-exp => procedure
; eval all args => a list of values
; apply that procedure to that list
(define (my-apply operator operands)
  (cond ((primitive? operator) ; check for type
         ; Scheme procedure (as before)
         (apply (get-scheme-procedure operator) operands))
        ((compound? operator)
         ; implement the idea of EM
         ; eval body w.r.t. a new env
         (my-eval (body operator)
                  ; binding operands passed in to the formal params
                  (extend-env-with-new-frame
                   (parameters operator)
                   operands
                   ; access to prev env
                   (env operator))))
        (error "operator not a procedure: " operator)))

;; ADT that implements the “double bubble” 
(define compound-tag 'compound)

; simply glue things in a list (not the only way)
(define (make-compound parameters body env)
  (list compound-tag parameters body env))

(define (compound? exp)  (tag-check exp compound-tag))
(define (parameters compound) (cadr compound))
(define (body compound)       (caddr compound))
(define (env compound)        (cadddr compound))