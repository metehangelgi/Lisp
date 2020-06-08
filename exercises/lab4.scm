#lang racket
(require rnrs/mutable-pairs-6)
(define nil '())
;;; --------------non-mutable stack---------------
#|
;constructor
(define (make-stack) nil)
(define (empty-stack? stack)(null? stack))

;selectors
(define (top stack)
  (if (empty-stack? stack)
      (error "stack underflow")
      (car stack)))
;operations
(define (insert stack elt)
  (cons elt stack))

(define (delete stack)
  (if (empty-stack? stack)
      (error "stack underflow")
      (cdr stack)))
|#

;;; --------------mutable stack---------------
;constructor
(define (make-stack)(mcons 'stack nil))

(define (stack? stack)
  (and (mpair? stack) (eq? 'stack (mcar stack))))

(define (empty-stack? stack)
  (if (not (stack? stack))
      (error "not a stack")
      (null? (mcdr stack))))

;selectors
(define (top stack)
  (if (empty-stack? stack)
      (error "stack underflow")
      (mcar (mcdr stack))))

;operations
(define (insert! stack elt)
  (cond ((not (stack? stack))
         (error "not stack"))
       (else
           (set-mcdr! stack (mcons elt (mcdr stack))))))

(define (delete! stack)
  (if (empty-stack? stack)
      (error "stack underflow - delete")
      (set-mcdr! stack (mcdr (mcdr stack)))))

;;; --------------unmutable queue---------------
#|
;constructor
(define (make-queue) nil)

(define (empty-queue? q)(null? q))

;selectors
(define (front-queue q)
  (if (empty-queue? q)
      (error "front of empty queue:")))

;operations
(define (delete-queue q)
  (if (empty-queue? q)
      (error "delete of empty queue:")
      (car q)))

(define (insert-queue q elt)
  (if (empty-queue? q)
      (cons elt nil)
      (cons (car q)(insert-queue (cdr q) elt))))
|#

;;; --------------mutable queue---------------
;constructor
(define (make-queue)
  (mcons 'queue (mcons nil nil)))

;selectors
(define (front-ptr q)
  (mcar (mcdr q)))

(define (rear-ptr q)
  (mcdr (mcdr q)))

;mutators
(define (set-front-ptr! q item)
  (set-mcar! (mcdr q) item))

(define (set-rear-ptr! q item)
  (set-mcdr! (mcdr q) item))

(define (queue? q)
  (and (mpair? q)(eq? 'queue (mcar q))))

(define (empty-queue? q)
  (if (not (queue? q))
      (error "empty queue")
      (null? (front-ptr q))))

(define (front-queue q)
  (if (empty-queue? q)
      (error "front empty queue")
      (mcar (front-ptr q))))

;operations
(define (insert-queue! q elt)
  (let ((new-pair (mcons elt nil)))
    (cond ((empty-queue? q)
            (set-front-ptr! q new-pair)
            (set-rear-ptr! q new-pair))
         (else
            (set-mcdr! (rear-ptr q) new-pair)
            (set-rear-ptr! q new-pair)))))

(define (delete-queue! q)
  (cond ((empty-queue? q)
        (error "empty queue"))
      (else
        (set-front-ptr! q (mcdr (front-ptr q))))))

