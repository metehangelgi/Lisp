(define xcor car)
(define ycor cdr)
(define make-vect cons)
(define make-seg list)
(define start-seg car)
(define end-seg cadr)



(define p1 (make-vect .25 0))
(define p2 (make-vect .35 .5))
(define p3 (make-vect .3 .6))
(define p4 (make-vect .15 .4))
(define p5 (make-vect 0 .65))
(define p6 (make-vect .4 0))
(define p7 (make-vect .5 .3))
(define p8 (make-vect .6 0))
(define p9 (make-vect .75 0))
(define p10 (make-vect .6 .45))
(define p11 (make-vect .1 .15))
(define p12 (make-vect 1 .35))
(define p13 (make-vect .75 .65))
(define p14 (make-vect .6 .65))
(define p15 (make-vect .65 .85))
(define p16 (make-vect .6 1))
(define p17 (make-vect .4 1))
(define p18 (make-vect .35 .85))
(define p19 (make-vect .4 .65))
(define p20 (make-vect .3 .65))
(define p21 (make-vect .15 .6))
(define p22 (make-vect 0 .85))


(xcor p1)
(ycor p1)


(define s1 (make-seg p1 p2))
(xcor (end-seg s1))

(define make-rect list)
(define origin car)
(define horiz cadr)
(define vert caddr)

(define make-vect list)
(define xcor car)
(define ycor cadr)

(define (+vect v1 v2)
  (make-vect
   (+ (xcor v1) (xcor v2))
   (+ (ycor v1) (ycor v2))
   ))

(define (scale-vect vect factor)
  (make-vect
   (* factor (xcor vect))
   (* factor (ycor vect))))

(define (-vect v1 v2)
  (+vect v1 (scale v2 -1)))


(define (rotate-vect v angle)
  (let ((c (cos angle))
        (s (sin angle)))
    (make-vect (- (* c (xcor v))
                  (* s (ycor v)))
               (+ (* c (ycor v))
                  (* s (xcor v))))))



(define (make-picture seglist)
  (lambda (rect)
    (for-each
     (lambda (segment)
            (let ((b (start-seg segment))
                  (e (end-seg segment)))
              (draw-line rect
                         (xcor b)
                         (ycor b)
                         (xcor e)
                         (xcor e))))
     seglist)))

;;;;;;;
(define wave-lines
  (list (make-seg p1 p2)
        (make-seg p2 p3)
        (make-seg p3 p4)
        (make-seg p4 p5)
        (make-seg p6 p7)
        (make-seg p7 p8)
        (make-seg p9 p10)
        (make-seg p10 p11)
        (make-seg p12 p13)
        (make-seg p13 p14)
        (make-seg p14 p15)
        (make-seg p15 p16)
        (make-seg p17 p18)
        (make-seg p18 p19)
        (make-seg p19 p20)
        (make-seg p20 p21)
        (make-seg p21 p22)
        ))
;;;;;;;

(define g (make-picture wave-lines)) ;;--------


(define (rotate90 pict)
  (lambda (rect)
    (pict (make-rect
           (+vect (origin rect)
                  (horiz rect))
           (vert rect)
           (scale-vect (horiz rect) -1)))))


(define (together pict1 pict2)
  (lambda (rect)
    (pict1 rect) (pict2 rect)))

;(draw (together
;       g
;       (rotate90 g)))


(define (beside pict1 pict2 a)
  (lambda (rect)
    (pict1 (make-rect
            (origin rect)
            (scale-vect (horiz rect) a)
            (vert rect)))
    (pict2 (make-rect
            (+vect
             (origin vect)
             (scale-vect (horiz rect) a))
            (scale-vect (horiz rect)
                        (- 1 a))
            (vert rect)))))


(define (above pict1 pict2 a)
  (rotate90
   (rotate90
    (rotate90
   (beside (rotate90 pict1)
           (rotate90 pict2)
           a)))))



;(define big-bro
;  (beside g
;          (above empty-picture g .5)
;          .5))

(define (flip pict)
  (lambda (rect)
    (pict (make-rect
           (+vect (origin rect)
                  (horiz rect))
           (scale-vect (horiz rect) -1)
           (vert rect)))))

(define acrobats
  (beside g
          (rotate90 (rotate90 (flip g)))
          .5))

(define 4bats
  (above acrobats
         (flip acrobats)
         .5))

(define (up-push pict n)
  (if (= n 0)
      pict
      (above (up-push pict (- n 1))
             pict .25)))

(define (right-push pict n)
  (if (= n 0) pict
      (beside pict
              (right-push pict (- n 1))
              .75)))

(define (corner-push pict n)
  (if (= n 0)
      pict
      (above
       (beside
        (up-push pict n)
        (corner-push pict (- n 1))
        .75)
       (beside
        pict
        (right-push pict (- n 1))
        .75)
       .25)))

(define (repeated rotate number pict)
  (if (zero? number)
      pict
      (repeated rotate (- number 1) pict)))

(define (4pict p1 r1 p2 r2 p3 r3 p4 r4)
  (beside
   (above
    (repeated rotate90 r1 p1)
    (repeated rotate90 r2 p2)
    .5)
   (above
    (repeated rotate90 r3 p3)
    (repeated rotate90 r4 p4)
    .5)
   .5))

(define (4same p r1 r2 r3 r4)
  (4pict p r1 p r2 p r3 p r4))

(define (square-limit pict n)
(4same (corner-push pict n)
       1 2 0 3))

(square-limit 4bats 2)