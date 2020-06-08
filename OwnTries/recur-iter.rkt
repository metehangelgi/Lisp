(define (my-length lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 (my-length (rest lst)))]))

(define (my-map f lst)
  (cond
    [(empty? lst) empty]
    [else (cons (f (first lst))
                (my-map f (rest lst)))]))


(my-length (list "a" "b" "c" "d"))

(define my-list (list "ready" "set" "go"))
(first my-list)

(my-map string-upcase my-list)
(my-map string-downcase my-list)

;(define (my-length)-iterative

(define (my-length lst)

    ; local function iter:

    (define (iter lst len)

      (cond

       [(empty? lst) len]

       [else (iter (rest lst) (+ len 1))]))

    ; body of my-length calls iter:

    (iter lst 0))

;iteration of length
(define (my-length lst)
  (define (iter lst len)
    (cond
      [(empty? lst) len]
      [else (iter (rest lst) (+ len 1))]))
  (iter lst 0))

(my-length (list "a" "b" "c" "d"))


(define your-list (list "me" "you" "he" "she" "it"))
;iteration of length
(define my-length (lambda (lst) (length-helper lst 0)))

(define length-helper (lambda (lst len)
                        (cond
                          [(empty? lst) len]
                          [else (length-helper (rest lst) (+ len 1))])))

(my-length your-list)




(define (find inlist what)
        (cond
            ((empty? inlist) false)
            ((= (first inlist) what) true)
            (else (find (rest inlist) what))))




  