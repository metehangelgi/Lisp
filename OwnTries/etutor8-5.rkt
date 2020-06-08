(define nil '())

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




;;; auto
;;;
(define (create-auto init-engine init-weight)
  (create-instance make-auto init-engine init-weight))

(define (make-auto self init-engine init-weight)
  (let ((root (make-root-object self)))
    ;; private state
    (define engine init-engine)  ; engine
    (define weight init-weight)  ; number
    (define driver nil)          ; person
    
    ;; methods
    
    ;; How fast a car can go depends on the power of the engine, its weight,
    ;; and the skill of its driver
    ;; Driving skill ranges from -10 to + 10, effectively adds or subtracts 
    ;; up to 10% of the car's weight.  + 10 is good driving.
    (define (speed-factor)
      (/ (ask engine 'HORSEPOWER) 
         (* weight
            (/ (- 100.0 (if driver (ask driver 'DRIVING-SKILL) 0.0)) 100.0))))
    
    ;; Change the driver
    (define (set-driver! new-driver)
      (set! driver new-driver))
    
    ;; message handling dispatch
    (lambda (message)
      (case message
        ;; required methods
        ((TYPE) (lambda () (type-extend 'named-object root)))
        
        ;; accessors (not necessarily one for every private field)
        ((DRIVER) (lambda () driver))
        ((ENGINE) (lambda () engine))
        ((WEIGHT) (lambda () weight))
        
        ;; mutators
        ((SET-DRIVER!) set-driver!)
        
        ;; operations
        ((SPEED-FACTOR) speed-factor)
        
        (else (get-method message root))))))


;;; Person
;;;
(define (create-person name)
  (create-instance make-person name))

(define (make-person self init-name)
  (let ((root (make-root-object self)))
    ;; private state
    (define name init-name)    ; string
    (define driving-skill 0)   ; number in range -10 .. + 10
    (define primary-car nil)   ; auto    
    
    ;; methods
    (define (set-driving-skill! new-skill)
      (set! driving-skill new-skill))
    (define (set-primary-car! new-auto)
      (set! primary-car new-auto))
    
    ;; message handling dispatch
    (lambda (message)
      (case message
        ;; required methods
        ((TYPE) (lambda () (type-extend 'named-object root)))
        
        ;; accessors
        ((NAME) (lambda () name))
        ((DRIVING-SKILL) (lambda () driving-skill))
        ((PRIMARY-CAR) (lambda () primary-car))
        
        ;; mutators
        ((SET-DRIVING-SKILL!) set-driving-skill!)
        ((SET-PRIMARY-CAR!) set-primary-car!)
        
        ;; operations
        
        (else (get-method message root))))))


;;; Engine
;;;
(define (create-engine init-cylinders init-hp)
  (create-instance make-engine init-cylinders init-hp))

(define (make-engine self init-cylinders init-hp)
  (let ((root (make-root-object self)))
    ;; private state
    (define num-cylinders init-cylinders)  ; positive integer
    (define horsepower init-hp)            ; positive number
    
    ;; methods
    (define (set-horsepower! new-horsepower)
      (set! horsepower new-horsepower))
    
    ;; message handling dispatch
    (lambda (message)
      (case message
        ;; required methods
        ((TYPE) (lambda () (type-extend 'named-object root)))
        
        ;; accessors
        ((NUM-CYLINDERS) (lambda () num-cylinders))
        ((HORSEPOWER) (lambda () horsepower))

        ;; mutators
        ((SET-HORSEPOWER!) set-horsepower!)

        ;; operations
        
        (else (get-method message root))))))



(define eric (create-person 'eric))
(define alfa (create-auto (create-engine 4 110) 1500))
(ask alfa 'set-driver! eric)
(ask eric 'set-primary-car! alfa)
(ask alfa 'speed-factor) ;=>0.07333333333333333
(ask eric 'set-driving-skill! 5)
(ask alfa 'speed-factor) ; => .07719298245614035

(define bubba (create-person 'bubba))
(define t-engine (create-engine 4 150))
(define truck (create-auto t-engine 4000))
(ask bubba 'set-driving-skill! -5)
(ask truck 'set-driver! bubba)
(ask truck 'speed-factor) ; => .03571428571428571