


(define (mix-it-up lst)
  (lambda (list)
                    (for-each
                    (lambda (element)
                      (cond ((= element 0) 0)
                            ((= element 1) 2)
                            ((= (remainder element 2) 1) (* 2 element))
                            (else (/ element 2))))
                    lst)))


(mix-it-up (list 1 2 3 4 5))
(mix-it-up (list 2 4 6 8))



(define mix-it-up (lambda (lst)
                    (let ((first (car lst)))
          (cond ((= first 0) (0 (mix-it-up (cdr lst))))
                ((= first) 1) (2 (mix-it-up (cdr lst)))
                ((remainder first 1) ((* 2 first) (cdr lst)))
                (else ((/ first 2) (cdr lst)))))))


(mix-it-up (list 3 34 45))