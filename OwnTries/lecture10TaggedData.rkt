(#%require racket)

(define (+c z1 z2)
  (make-coplex-from-rect (+ (real z1) (real z2) )
                         (+ (imag z1) (imag z2))))

(define (*c z1 z2)
        (make-complex-from-polar (* (mag z1) (mag z2))
                                 (+ (angl z1) (angl z2))))

(define (make-complex-from-rect rl im) (list rl im))

;Bert's Data 
(define (make-complex-from-polar mg an) 
  (list (* mg (cos an))
        (* mg (sin an))))

(define (real cx) (car cx))
(define (imag cx) (cdr cx))
(define (mag cx) (sqrt (+ (sqr (real cx))
                          (sqr (imag cx)))))

(define (angl cx) (atan (imag cx) (real cx)))


;Ernie's Data
(define (make-complex-from-rect rl im)
  (list (sqrt (+ (sqr rl) (sqr im)))
        (atan im rl)))


(define (make-complex-from-polar mg an) (list mg an))
(define (rl cx) (* (mag cx) (cos (angl cx))))
(define (im cx) (* (mag cx) (sin (angl cx))))
(define (mag cx) (car cx))
(define (angl cx) (cadr cx))


;for both
(define (make-complex-from-rect rl im)
  (list 'rect rl im))
(define (make-complex-from-polar mg an)
  (list 'polar mg an))
(define (tag obj) (car obj))
(define (contents obj) (cdr obj))


(define (real z)
  (cond ((eq? (tag z) 'rect) (car (contents z)))
        ((eq? (tag z) 'polar) (* (car (contents z));mag
                                 (cos (cadr (contents z)))));angle
        (else (error "unknown form of object"))))


;consept of tag

(define (make-point x y) (list 'point x y))




(define (make-sum addend augend)
  (list '+ addend augend))


(define (sum-exp? e)
  (and (pair? e) (eq? (car e) '+)))


(define (sum-augend sum) (cadr sum))
(define (sum-addend sum) (caddr sum))

(define (eval-1 exp)
  (cond
    ((number? exp) exp)
    ((sum-exp? exp)
     (+ (eval-1 (sum-addend exp))
        (eval-1 (sum-augend exp))))
    (else (error "unknown expression" exp))))

(define exp1 (make-sum (make-sum 3 15) 20)); sonrasında verdiği için hata veriyo onu öğren !

exp1

;(eval-1 exp) --- unknown expression #<procedure:exp>

(eval-1 (make-sum 4 (make-sum 3 5)))




;eval-2

(define (make-range-2 min max) (list min max))

(define (range-min-2 range) (car range))
(define (range-max-2 range) (cdr range))

(define (range-add-2 r1 r2)
  (make-range-2
   (+ (range-min-2 r1) (range-min-2 r2))
   (+ (range-max-2 r1) (range-max-2 r2))))

(define (eval-2 exp)
  (cond
    ((number? exp) exp)
    ((sum-exp? exp)
     (let ((v1 (eval-2 (sum-addend exp)))
           (v2 (eval-2 (sum-augend exp))))
       (if (and (number? v1) (number v2))
           (+ v1 v2)
           (range-add-2 v1 v2))))
    ((pair? exp) exp)
    (else (error "unknown expression" exp))))

;eval-3
(define sum-tag '+)
(define (make-sum addend augend)
  (list sum-tag addend augend))

(define (sum-exp? e)
  (and (pair? e) (eq? (car e) sum-tag)))

(define constant-tag 'const)

(define (make-constant val)
  (list constant-tag val))

(define (constant-exp? e)
  (and (pair? e) (eq? (car e) constant-tag)))

(define (constant-val const) (cadr const))

(define (eval-3 exp)
  (cond
    ((constant-exp? exp) (constant-val exp))
    ((sum-exp? exp)
     (+ (eval-3 (sum-addend exp))
        (eval-3 (sum-augend exp))))
    (else (error "unkonwn expr type:" exp))))

(eval-3 (make-sum (make-constant 3)
                  (make-constant 5)))

;eval-4

(define (eval-4 exp)
  (cond
  ((constant-exp? exp) exp)
  ((sum-exp? exp)
   (make-constant
    (+ (constant-val (eval-4 (sum-addend exp)))
       (constant-val (eval-4 (sum-augend exp))))))
  (else (error "unknown expr type: " exp))))

(eval-4 (make-sum (make-constant 3)
                  (make-constant 5)))


(define (constant-add c1 c2)
  (make-constant (+ (constant-val c1)
                    (constant-val c2))))

(define (eval-4 exp)
  (cond
    ((constant-exp? exp) exp)
    ((sum-exp? exp)
     (constant-add (eval-4 (sum-addend exp))
                   (eval-4 (sum-augend exp))))
    (else (error "unkown expr type: " expr))))



(eval-4 (make-sum (make-constant 3)
                  (make-constant 5)))


;eval-5

(define range-tag 'range)

(define (make-range min max)
  (list range-tag min max))

(define (range-exp? e)
  (and (pair? e) (eq? (car e) range-tag)))

(define (range-min range) (cadr range))
(define (range-max range) (caddr range))

(define (eval-5 exp)
  (cond
    ((constant-exp? exp) exp)
    ((range-exp? exp) exp)
    ((sum-exp? exp)
    (let ((v1 (eval-5 (sum-addend exp)))
          (v2 (eval-5 (sum-augend exp))))
      (if (and (constant-exp? v1) (constant-exp? v2))
          (constant-add v1 v2)
          (range-add (val2range v1) (val2range v2)))))
    (else (error "unknown expr type: " exp))))

;eval-6
(define (value-exp? v)
  (or (constant-exp? v) (range-exp? v)))


(define (value-add-6 v1 v2)
  (if (and (constant-exp? v1) (constant-exp? v2))
      (constant-add v1 v2)
      (range-add (val2range v1) (val2range v2))))


(define (val2range val)
  (cond
    ((range-exp? val) val)
    (else (make-range val val))))

(define (eval-6 exp)
  (cond
    ((value?exp? exp) exp)
    ((sum-exp? exp)
     (value-add-6 (eval-6 ((sum-addend exp)))
                  (eval-6 ((sum-augend exp)))))
    (else (error "unknown expr type: " exp))))

;eval-7

(define limited-tag 'limited)
(define (make-limited-precision val err)
  (list limited-tag val err))

(define (limited-exp? exp)
  (eq? (car exp) 'limited-tag))

(define (eval-7 exp)
  (cond
    ((value-exp? exp) exp)
    ((limited-exp? exp) exp)
    ((sum-exp? exp)
     (value-add-6 (eval-7 (sum-addend exp))
                  (eval-7 (sum-augend exp))))
    (else (error "unknown expr type: " exp))))

;(eval-7 (make-sum
;         (make-range 4 6)
;         (make-limited-precision 10 1)))----unknown expr type:  (limited 10 1)


(define (value-add-7 v1 v2)
  (cond
    ((and (constant-exp? v1) (constant-exp? v2))
     (constant-add v1 v2))
    ((and (value-exp? v1) (value-exp? v2))
     (range-add (val2range v1) (val2range v2)))
    (else (error "unknown expr : " v1 " or " v2))))

(define (eval-7 exp)
  (cond
    ((value-exp? exp) exp)
    ((limited-exp? exp) exp)
    ((sum-exp? exp)
     (value-add-7 (eval-7 (sum-addend exp))
                  (eval-7 (sum-augend exp))))
    (else (error "unknown expr type: " exp))))

;(eval-7 (make-sum
;         (make-range 4 6)
;         (make-limited-precision 10 1)))  ----unknown expr type:  (limited 10 1)