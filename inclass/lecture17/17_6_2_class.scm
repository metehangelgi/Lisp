;; Environment model (part of eval 6)
;; Environment = list<table>

(load "lec17_6.scm")
(define nil '())

(define (extend-env-with-new-frame names values env)
  (let ((new-frame (make-table)))
    (make-bindings! names values new-frame)
    (cons new-frame env)))

(define (make-bindings! names values table)
  (for-each
   (lambda (name value)
     (table-put! table name value))
   names values))

; the initial global environment
; contains bindings for built in procedures.
(define GE
  (extend-env-with-new-frame
   (list 'plus* 'greater*)
     (list (make-primitive +) (make-primitive >))
     nil))

; lookup searches the list of frames for the first match
; first in the first frame of the sequence
; if no binding there
; => recursively down to the remainder of the env 
(define (lookup name env)
  (if (null? env)
      (error "unbound variable: " name)
      ;;
      ))

; define changes the first frame in the environment
(define (eval-define exp env)
  (let ((name (cadr exp))
        (defined-to-be (caddr exp)))
    ;;
    'undefined))

(my-eval '(define* twice* (lambda* (x*) (plus* x* x*))) GE)
(my-eval '(twice* 4) GE)