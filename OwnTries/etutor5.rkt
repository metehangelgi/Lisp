(+ '1 '2)



(define x 3)



(define b 'x)

'b

(define a x)
a
(quote (quote a))
(quote a)
''a



(define memqq (lambda (word text)
               (cond ((null? text) #f)
                     ((equal? word (car text)) text)
                     (else (memqq word (cdr text))))))


(memqq 'fred '(arnold fred beth bill fred grant jillian))
(memqq 'eric '(arnold fred beth bill fred grant jillian))


(define *english-to-french*
        '((cat chat) (cake gateau) (present cadeau) (I je) (eat mange) 
          (the le)))

(define lookup
  (lambda (word dictionary)
    (cond ((null? dictionary) #f)
          ((equal? word (car (car dictionary))) (cadr (car dictionary)))
          (else (lookup word (cdr dictionary))))))

(lookup 'cake *english-to-french*)

(define translate (lambda (sentence dictionary)
                     (define (look word)
                      (if (null? word)
                          '()
                          (lookup word dictionary)))
                    (map look sentence)))

(translate '(I eat the cat) *english-to-french*)


;;; Given a document, return a histogram
;;; List < symbol > -> histogram

(define make-histogram
  (lambda (document)
    (add-words-to-histogram document (make-empty-histogram))))

;;; Make a new histogram with no words in it
;;; null -> histogram

(define make-empty-histogram
  (lambda () '()))

;;; Given a word and a histogram, return #t if the word is in the histogram,
;;; otherwise return #f

;;; symbol,histogram->boolean
(define in-histogram? 
  (lambda (word histogram)
    (and (pair? histogram)
	 (or (eq? word (car (car histogram)))
	     (in-histogram? word (cdr histogram))))))

;;; Add a new word to a histogram with a count of 1
;;; word,histogram->histogram

(define add-new-word-to-histogram
  (lambda (word histogram)
    (cons (list word 1) histogram)))

(define increment-word-count-in-histogram
  (lambda (word histogram)
    (if (equal? word (car (car histogram)))
        (set-cdr! (car histogram)  (cons (+ 1 (car (cdr (car histogram)))) '() ) )
        (increment-word-count-in-histogram word (cdr histogram)) )
    histogram
    ))

(define histogram '((dog 3) (cat 9) (pig 2) ))

(increment-word-count-in-histogram 'cat histogram)

(define add-words-to-histogram
  (lambda (words histogram)
    
    (define check (lambda (word histogram)
      (if (in-histogram? word histogram)
          (increment-word-count-in-histogram word histogram)
          (add-new-word-to-histogram word histogram)) 
    ))
    (if (not (null? words))
         (add-words-to-histogram (cdr words) (check (car words) histogram))
         histogram)
    ))


(add-words-to-histogram '(cat cat) '((dog 6) (cat 3)))
(add-words-to-histogram '(cat dog cat) '())


(define times-occuring
  (lambda (histogram word)
    (cond ((null? histogram) 0)
          ((equal? word (car (car histogram))) (cadr (car histogram)))
          (else (times-occuring (cdr histogram) word)))))


(times-occuring
  '((or 1)
    (harder 1)
    (it 1)
    (is 1)
    (said 1)
    (toot 3)
    (tooters 2)
    (two 3)
    (to 6)
    (tried 1)
    (flute 1)
    (the 3)
    (tooted 1)
    (who 1)
    (tutor 4)
    (a 1))
  'tutor)

(list 1 2)
(define ben (cons 2 3))


(cdr ben)

(define (make-complex-from-rect rl im) (cons rl im))
(define (make-complex-from-polar mg an)
  (list (* mg (cos an))
        (* mg (sin an))))
(define (real cx) (car cx))
(define (imag cx) (cadr cx))
(define (mag cx) (sqrt (+ (square (real cx)) (square (imag cx)))))
(define (anggle cx) (atan (imag cx) (real cx)))



(define cx (make-complex-from-rect 3 2) )

(define (imag cx) (cdr cx))

(imag cx)


(define range-add (lambda (r1 r2)
    (make-range (+ (range-min r1) (range-min r2))
                (+ (range-max r1) (range-max r2)))))

  (define range-tag 'range)

  (define make-range (lambda (lower upper)
    (list range-tag lower upper)))

  (define range-min cadr)
  (define range-max caddr)


 (range-add (make-range 4 6) (make-range 6 4))

