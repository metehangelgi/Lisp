(define (list->cycle lst)
  (set-cdr! (last lst) lst)
  lst)


(define (last lst)
  (if (null? lst)
      (error "not long enough")
      (if (null? (cdr lst))
          lst
          (last (cdr lst))
          )))

(define test-cycle (list->cycle '(a b c d)))
test-cycle
(define (rotate-left cycle)
  (cdr cycle))

(define (rotate-right cycle)
  (define (aux where start)
    (if (eq? (cdr where) start)
        where
        (aux (cdr where) start)))
  (aux cycle cycle))

(define (delete-cycle! cycle)
  (set-cdr! (rotate-right cycle) (rotate-left cycle))
  (set-cdr! cycle '())
  'done)


;(delete-cycle! test-cycle)
test-cycle

(define (insert-cycle! new cycle)
  (let ((new-cell (list new)))
    (set-cdr! new-cell cycle)
    (set-cdr! (rotate-right cycle) new-cell))
  'done)


(insert-cycle! (rotate-left (rotate-left test-cycle)) 'x)

test-cycle


(define (get-values lst dir)
  (if (not lst)
      ’()
      (cons (dcar lst)
            (get-values (lst dir) dir))))