;;;
;;; Basic object system in Scheme
;;;

;;------------------------------------------------------------
;; Instance

; instance is an object which holds the "self" of a normal
; object instance.  It handles type requests, but otherwise
; passes all messages along to the handler procedure that it
; contains.
;
(define (make-instance)
  (let ((handler #f))
    (lambda (message)
      (case message
        ((SET-HANDLER!)
         (lambda (handler-proc)
           (set! handler handler-proc)))
        (else (get-method message handler))))))

; to create an instance 
;
(define (create-instance maker . args)
  (let* ((instance (make-instance))
         (handler (apply maker instance args)))
    (ask instance 'SET-HANDLER! handler)
    (if (method? (get-method 'INSTALL instance))
        (ask instance 'INSTALL))
    instance))

;;------------------------------------------------------------
;; Root Object


; Root object.  It contains the TYPE and IS-A methods.
; All classes should inherit (directly or indirectly) from root.
;
(define (make-root-object self)
  (lambda (message)
    (case message
      ((TYPE)
       (lambda () '(root)))
      ((IS-A)
       (lambda (type)
         (if (memq type (ask self 'TYPE)) #t #f)))
      (else
       (no-method)))))

;;------------------------------------------------------------
;; Object Interface

; ask
; 
; We "ask" an object to invoke a named method on some arguments.
;
(define (ask object message . args)
  ;; See your Scheme manual to explain `. args' usage
  ;; which enables an arbitrary number of args to ask.
  (let ((method (get-method message object)))
    (cond ((method? method)
           (apply method args))
          (else
           (error "No method for" message 'in 
                  (safe-ask 'UNNAMED-OBJECT
                            object 'NAME))))))

; Safe (doesn't generate errors) method of invoking methods
; on objects.  If the object doesn't have the method,
; simply returns the default-value.  safe-ask should only
; be used in extraordinary circumstances (like error handling).
;
(define (safe-ask default-value obj msg . args)
  (let ((method (get-method msg obj)))
    (if (method? method)
        (apply ask obj msg args)
        default-value)))

;;--------------------
;; Method Interface
;;
;; Objects have methods to handle messages.

; Gets the indicated method from the object or objects.
; This procedure can take one or more objects as 
; arguments, and will return the first method it finds 
; based on the order of the objects.
;
(define (get-method message . objects)
  (define (try objects)
    (if (null? objects)
        (no-method)
        (let ((method ((car objects) message)))
          (if (not (eq? method (no-method)))
              method
              (try (cdr objects))))))
  (try objects))

(define (method? x)
  (cond ((procedure? x) #T)
        ((eq? x (no-method)) #F)
        (else (error "Object returned this non-message:" x))))

(define no-method
  (let ((tag (list 'NO-METHOD)))
    (lambda () tag)))

; Use this inside each make-<object> class definition,
; in the TYPE method for that class, in order to add 
; to the type information for that new class.
;
(define (type-extend type . parents)
  (cons type 
        (remove-duplicates
         (append-map (lambda (parent) (ask parent 'TYPE))
                     parents))))

;;------------------------------------------------------------
;; Utility procedures

(define (random-number n)
  ;; Generate a random number between 1 and n
  (+ 1 (random n)))

(define (pick-random lst)
  (if (null? lst)
      #F
      (list-ref lst (random (length lst)))))

(define (delq item lst)
  (cond ((null? lst) '())
        ((eq? item (car lst)) (delq item (cdr lst)))
        (else (cons (car lst) (delq item (cdr lst))))))

(define (filter predicate lst)
  (cond ((null? lst) '())
        ((predicate (car lst))
         (cons (car lst) (filter predicate (cdr lst))))
        (else (filter predicate (cdr lst)))))

(define (fold-right op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (fold-right op init (cdr lst)))))

(define (remove-duplicates lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (remove-duplicates (filter (lambda (x) 
                                         (not (eq? x (car lst))))
                                       lst)))))



(define (create-A state)
  (create-instance make-A state))

(define (make-A self state)
  (let ((root (make-root-object self)))
    (lambda (msg)
      (case msg
        ((GETSTATE) (lambda () state))
        ((ALTER) (lambda (newstate)
                   (set! state newstate)
                   (ask self 'GETSTATE)))
        ((REPORT) (lambda ()
                    (list (ask self 'GETSTATE))))
        (else (get-method msg root))))))


(define (create-B state)
  (create-instance make-B state))

(define (make-B self state)
  (let ((my-A (make-A self state)))
    (lambda (msg)
      (case msg
        ((ALTER) (lambda (newstate)
                   (set! state newstate)
                   (ask self 'GETSTATE)))
        ((REPORT) (lambda ()
                    (cons state (ask my-A 'REPORT))))
        (else (get-method msg my-A))))))


(define (create-C state)
  (create-instance make-C state))

(define (make-C self state)
  (let ((my-A (make-A self state))
        (my-B (make-B self state)))
    (lambda (msg)
      (case msg
        ((GETSTATE) (lambda () (ask my-A 'GETSTATE)))
        ((ALTER) (lambda (newstate)
                    (ask my-A 'ALTER newstate)))
        ((REPORT) (lambda ()
                    (ask my-B 'REPORT)))
        (else (get-method msg my-A))))))

(define alex (create-A 'alpha))
(define bruce (create-B 'beta))
(define chris (create-C 'gamma))