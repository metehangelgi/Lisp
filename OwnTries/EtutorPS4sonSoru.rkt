(define (accumulate op init seq)
  (define (iter ans rest)
    (if (null? rest)
        ans
        (iter (op ans (car rest))
              (cdr rest))))
  (iter init seq))

(define (flatten seq)
  (accumulate append '() seq))

(define (flatmap op seq)
  (flatten (map op seq)))

(define (atom? x)
  (not (pair? x)))

(define (paths tree)
  (if (atom? tree)
      (list (list tree))
      (flatmap (lambda (node)
                 (map (lambda (path)
                        (cons (car tree) path))
                      (paths node)))
               (cdr tree))))


(paths '(1 2 (3) (2 3 4) (((5)))))