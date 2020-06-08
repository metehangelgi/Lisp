;; Table implementation
; make-table         void -> table
; table-get          table, symbol -> (binding | null)
; table-put!         table, symbol, anytype -> undef
; binding-value      binding -> anytype

(define (make-table)
  (list 'table*))

(define (find key bindings)
  (cond ((null? bindings) #f)
	((equal? (caar bindings) key) (car bindings))
	(else (find key (cdr bindings)))))

(define (table-get table key)
  (find key (cdr table)))

(define (binding-value binding)
  (cdr binding))

(define (table-put! table key value)
  (let ((binding (find key (cdr table))))
    (if binding
	(set-cdr! binding value)
	(set-cdr! table (cons (cons key value) (cdr table))))))