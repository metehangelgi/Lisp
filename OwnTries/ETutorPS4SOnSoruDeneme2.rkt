(#%require racket)
(define nil empty) 



(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (reduce op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (reduce op init (cdr lst)))))

(define fold-right reduce)


(define (flatten-branch branch)
  (if (pair? branch) 
      (flatten-tree branch)
      (list branch)))

(define (flatten-tree tree)
  (if (equal? tree '(())) nil
  (fold-right append 
              nil 
              (map flatten-branch tree))))


(flatten-tree '((1) (2 3 4) (((5)))))

(define (leaf? x)
  (not (pair? x)))

(flatten-tree '(()))
(null? '(()))
(flatten-tree '((1) (()) (((5)))))

(define flatten-tree
  (lambda (tree)
    (if (null? tree)
      tree
      (if (leaf? tree)
        (list tree)
        (fold-right append nil (map flatten-tree tree))))))



(define fold-right-tree (lambda (tree)
                            (sum-list (flatten-tree tree))
                          ))

(list? (flatten-tree '((1) (2 3 4) (((5))))))

(fold-right-tree '((1) (2 3 4) (((5)))))


(define (reduce op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (reduce op init (cdr lst)))))

(define fold-right reduce)

(define flatten-tree
  (lambda (tree)
    (if (null? tree)
      tree
      (if (leaf? tree)
        (list tree)
        (fold-right append nil (map flatten-tree tree))))))


(define (sum-list lst)
  (if (null? lst) 0
      (+ (car lst) (sum-list (cdr lst)))
    ))

(define (sum num1 num2)
  (+ num1 num2)
  )
  
(define (fold-right-tree op init tree)
    (lambda (tree)
      (if (null? tree)
          0
          (if (leaf? tree)
              (sum-list (list tree))
              (fold-right sum nil (map flatten-right-tree tree))))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      empty
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (fold-right-tree op init tree)
      (if (null? tree)
          0
          (if (leaf? tree)
              (sum-list (list tree))
              (+ (fold-right op init (map car tree)) (fold-right op init (map cdr tree))))))




