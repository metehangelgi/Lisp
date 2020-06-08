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

;;; 
;;; Safe (As in -- where you put your valuables) Abstractions
;;;

;; named-object
;;
(define (create-named-object name)      ; symbol -> named-object
  (create-instance make-named-object name))

(define (make-named-object self name)
  (let ((root-part (make-root-object self)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'named-object root-part)))
        ((NAME) (lambda () name))
        (else (get-method message root-part))))))
  
;; container
;;
;; A container holds THINGS.  
;; 
;; This class is not meant for "stand-alone" objects; rather, 
;; it is expected that other classes will inherit from the
;; container class in order to be able to contain things. 
;; For this reason, there is no create-container procedure.

(define (make-container self)
  (let ((root-part (make-root-object self))
        (things nil)) ; a list of THING objects in container
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'container root-part)))
        ((THINGS) (lambda () things))
        ((HAVE-THING?)
         (lambda (thing)  ; container, thing -> boolean
           (not (null? (memq thing things)))))
        ((ADD-THING)
         (lambda (new-thing)
           (if (not (ask self 'HAVE-THING? new-thing))
               (set! things (cons new-thing things)))
           'DONE))
        ((DEL-THING)
         (lambda (thing)
           (set! things (delq thing things))
           'DONE))
        (else (get-method message root-part))))))

(define (delq item lst)
  (cond ((null? lst) '())
        ((eq? item (car lst)) (delq item (cdr lst)))
        (else (cons (car lst) (delq item (cdr lst))))))

;; safe
;;
;; symbol, number -> safe
(define (create-safe name combo)
  (create-instance make-safe name combo))

(define (make-safe self name combo)
  (let (;; Superclasses
	(named-part (make-named-object self name))
	(container-part (make-container self))
	;; Additional local state
	(locked? #t))          ; boolean
    (lambda (message)
      (case message
        ((TYPE) (lambda ()
		  (type-extend
		   'safe named-part container-part)))
	((LOCKED?) (lambda () locked?))
	((LOCK) (lambda () (set! locked? #t)))
	((UNLOCK) (lambda (purported-combo)
		    (if (= purported-combo combo)
			(begin (set! locked? #f)
			       #t)
			#f)))
	((ADD-THING) ;; return #t if success, else #f
	 (lambda (new-thing)
	   (if (ask self 'LOCKED?)
	       #f
	       (begin
		 (ask container-part 'ADD-THING new-thing)
		 #t))))
	((DEL-THING)  ;; return #t if success, else #f
	 (lambda (old-thing)
	   (if (or (ask self 'LOCKED?)
		   (not (ask self 'HAVE-THING? old-thing)))
	       #f
	       (begin
		 (ask container-part 'DEL-THING old-thing)
		 #t))))
	(else (get-method message named-part container-part))))))

(define (create-limited-capacity-safe name combo capacity)
  (create-instance make-limited-capacity-safe name combo capacity))

(define (list-length lst) (vector-length (list->vector lst)))

(define (make-limited-capacity-safe self name combo capacity)
  (let ((safe (make-safe self name combo)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'limited-capacity-safe safe)))
        ((HOW-MANY-THINGS?) (lambda () (list-length (ask safe 'things))))
        ((FULL?) (lambda () (= (ask self 'how-many-things?) capacity)))
        ((ADD-THING) 
         (lambda (new-thing)
           (if (ask self 'full?) #f
               (ask safe 'add-thing new-thing))))
        (else (get-method message safe))))))

(define vault (create-safe 'bank-vault 123))