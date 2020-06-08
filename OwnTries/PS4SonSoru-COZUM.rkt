(#%require racket)
(define nil empty) 

(define (reduce op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (reduce op init (cdr lst)))))

(define fold-right reduce)

(define (leaf? x)
  (not (pair? x)))

(define flatten-tree
  (lambda (tree)
    (if (null? tree)
      tree
      (if (leaf? tree)
        (list tree)
        (fold-right append nil (map flatten-tree tree))))))


(define (fold-right-tree op id tree)
  (fold-right op id (flatten-tree tree)))

(fold-right-tree + 0 '((1) (2 3 4) (((5)))))
