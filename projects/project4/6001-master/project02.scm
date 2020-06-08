(load "mind-read.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Part 1: A Simple Mind-Reading Machine
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(always-true (list #t #f #t #f))
;(always-false (list #t #f #t #f)) 
;(guess-last (list #t #f #t #f))
;(guess-not-last (list #t #f #t #f))
;(alternating-true-false (list #t #f #t #f))

;;;;;;;;;;;;;;;;;
; Question 1: Winning Strategies for each function
;;;;;;;;;;;;;;;;;

; always-true
;ROUNDS: 10
;YOUR SCORE: 10
;MY SCORE: 0
;HISTORY: (#f #f #f #f #f #f #f #f #f #f)

; always-false
;ROUNDS: 10
;YOUR SCORE: 10
;MY SCORE: 0
;HISTORY: (#t #t #t #t #t #t #t #t #t)

;guess-last
;You said: #t, I predicted: #f I lose :-(
;ROUNDS: 10
;YOUR SCORE: 10
;MY SCORE: 0
;HISTORY: (#t #f #t #f #t #f #t #f #t #f)
;ENTER t, f, or e to end the game

; guess-not-last
;You said: #f, I predicted: #t I lose :-(
;ROUNDS: 10
;YOUR SCORE: 10
;MY SCORE: 0
;HISTORY: (#f #f #f #f #f #f #f #f #f #f)
;ENTER t, f, or e to end the gamee

; alternating-true-false
;You said: #t, I predicted: #f I lose :-(
;ROUNDS: 10
;YOUR SCORE: 10
;MY SCORE: 0
;HISTORY: (#t #f #t #f #t #f #t #f #t #f)
;ENTER t, f, or e to end the game

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Part 2: Creating More Functions
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;
; Question 2: Skip most recent
;;;;;;;;;;;;;;;;;

(define (skip-most-recent f)
  (lambda (x)
    (if (or (null? x) (= (length x) 1))
        #t
        (f (cdr x)))))

(display "Question 2 test cases") (newline)
((skip-most-recent guess-last) (list #t #f #t #f))  ; should equal #f

;;;;;;;;;;;;;;;;;
; Question 3: Skip n most recent
;;;;;;;;;;;;;;;;;

(define (skip-most-recent-n n f)
  (if (= n 1)
        (skip-most-recent f)
        (skip-most-recent-n (- n 1) (skip-most-recent f))))

(display "Question 3 test cases") (newline)
((skip-most-recent-n 10 guess-last) (list #f #f #f #t #t #t)) ; should return #t
((skip-most-recent-n 2 guess-last) (list #f #f #f #t #t #t)) ; should return #f
((skip-most-recent-n 3 guess-last) (list #f #f #f #t #t #t)) ; should return #t


;;;;;;;;;;;;;;;;;
; Question 4: Negation
;;;;;;;;;;;;;;;;;

(define (negation f)
  (lambda (x)
    (not (f x))))

(display "Question 4 test cases") (newline)
((negation (skip-most-recent-n 10 guess-last)) (list #f #f #f #t #t #t)) ;should return #f
((negation (skip-most-recent-n 2 guess-last)) (list #f #f #f #t #t #t)) ; should return #t
((negation (skip-most-recent-n 3 guess-last)) (list #f #f #f #t #t #t)) ; should return #f

;;;;;;;;;;;;;;;;;
; Question 5: n-in-a-row
;;;;;;;;;;;;;;;;;

(define (n-in-a-row n)
  ; input: integer n > 0
  ; output: function that returns true if n pervious guesses are true
  (define n-in-a-row-h
    (lambda (n x)
      (cond ((< (length x) n) #t)
            ((guess-not-last x) #f)
            ((= n 1) (guess-last x))
            (else (n-in-a-row-h (- n 1) (cdr x))))))
  (lambda (x)
    (n-in-a-row-h n x)))

(display "Question 5 test cases") (newline)
((n-in-a-row 4) (list #t #t #t #t #f #f)) ;show return #t 
((n-in-a-row 5) (list #t #t #t #t #f #f)) ;show return #f
((n-in-a-row 7) (list #t #t #t #t #f #f)) ;show return #t

;;;;;;;;;;;;;;;;;
; Question 6: fand
;;;;;;;;;;;;;;;;;

(define (fand f g)
  (lambda (x)
    (and (f x) (g x))))

(display "Question 6 test cases") (newline)
((fand (n-in-a-row 4) (n-in-a-row 5)) (list #t #t #t #t #f #f)) ; should return #f
((fand (n-in-a-row 4) (n-in-a-row 7)) (list #t #t #t #t #f #f)) ; should return #t


;;;;;;;;;;;;;;;;;
; Question 7: true-but-not-always-true
;;;;;;;;;;;;;;;;;

(define true-but-not-always-true
  (lambda (x)
    (cond ((< (length x) 3) #f)
          ; if length x < 3, return #f
          (((fand (n-in-a-row 1) (negation (n-in-a-row 3))) x) #t)
          ; if most recent choice is #t but three most recent are not all true, return #t
          (else #f)
          ; otherwise, return #f
          )))

(display "Question 7 test cases") (newline)
(true-but-not-always-true (list #t #f #f)) ; should return #t
(true-but-not-always-true (list #t #t #f #t #f #t)) ; should return #t
(true-but-not-always-true (list #t #f)) ; should return #f
(true-but-not-always-true (list #t #t #t)) ; should return #f


;;;;;;;;;;;;;;;;;
; Question 8: generate
;;;;;;;;;;;;;;;;;

(define (generate f n)
    (define (gen-iter f n history)
      (if (= n 0)
          history
          (gen-iter f (- n 1) (cons (f history) history))))
  (reverse (gen-iter f n '())))

(display "Question 8 test cases") (newline)
(display (generate always-true 10)) (newline) ; should return (#t #t #t #t #t #t #t #t #t #t)
(display (generate always-false 10)) (newline) ; should return (#f #f #f #f #f #f #f #f #f #f)
(display (generate guess-not-last 10)) (newline) ; should return (#t #f #t #f #t #f #t #f #t #f)

;;;;;;;;;;;;;;;;;
; Question 9: test cases for batch-simple-game
;;;;;;;;;;;;;;;;;

(batch-simple-game (generate guess-not-last 10) guess-not-last) ; should return 0

;;;;;;;;;;;;;;;;;
; Question 10: bad-random-choice
;;;;;;;;;;;;;;;;;

(define (bad-random-choice f1 f2 p)
  (let ((q (random-fraction)))
    (if (<= q p)
        (lambda (x) (f1 x))
        (lambda (x) (f2 x)))))

(display (generate (bad-random-choice always-true always-false 0.5) 20)) (newline)

; bad-random-choice is "bad" because it does not return a random function. 
; Instead, it randomly chooses a non-random function, and returns that. 

;;;;;;;;;;;;;;;;;
; Question 11: random-choice
;;;;;;;;;;;;;;;;;
(define (random-choice f1 f2 p)
  ; input: functions f1 and f2, and 0 <= p <= 1
  ; output: (f1 x) with probabilty p and (f2 x) with probability (1 - p)
  (lambda (x)
    (let ((q (random-fraction)))
      (if (<= q p)
          (f1 x)
          (f2 x)))))

(display (generate (random-choice always-true always-false 0.5) 20)) (newline)


;;;;;;;;;;;;;;;;;
; Question 12: last-2n
;;;;;;;;;;;;;;;;;

(define (last-2n x)
  (define (get-2n-h n)
    (if (< (random-fraction) 0.5)
        n
        (get-2n-h (+ n 1))))
  (let ((n (get-2n-h 1)))
    (if (= n 1)
        (guess-last x)
        ((skip-most-recent-n n guess-last) x))))

(display "Test case for Question 12") (newline)
(display (last-2n (list #t #t #t #f #f #f))) (newline) ; should return #t 1/2 + 1/4 + 1/8 of the time
(display (last-2n (list #t #f #f #f #f))) (newline) ; should return #t 1/2 of the time

;;;;;;;;;;;;;;;;;
; Question 13: lastn
;;;;;;;;;;;;;;;;;

(define (lastn n)
  (define (lastn-h n x t f)
    (cond ((or (null? x) (= n 0)) (>= (- t f) 0))
          ; if x is null or n is 0, return #t if t > f, false otherwise
          ((car x) (lastn-h (- n 1) (cdr x) (+ t 1) f))
          ; if next x is #t, increase t by 1
          (else (lastn-h (- n 1) (cdr x) t (+ f 1)))
          ; otherwise, increase f by 1
          ))
  
  (lambda (x)
    (lastn-h n x 0 0)))

(display "Test cases for Question 13") (newline)
((lastn 3) (list #t #t #f)) ; should return #t
((lastn 4) (list #t #t #f #f)) ; should return #t
((lastn 5) (list #t #t #f #f)) ; should return #t
((lastn 5) (list #t #f #f #f)) ; should return #f


;;;;;;;;;;;;;;;;;
; Question 14: prediction
;;;;;;;;;;;;;;;;;

(define (prediction funcs weights history)
  (define (calc-votes funcs history)
    ; apply each func in funcs to history
    ; and return results in a list
    (map (lambda (f) (f history)) funcs))
  
  (define (to-plus-minus votes)
    ; convert #t to 1 and #f to -1
    (map (lambda (x) (if x 1 -1)) votes))
  
  (define (weighted-vote-sum plus-minus weights)
    ; input: list of -1/1, list of weights from 0 to 1 of same length
    ; output: weighted some of -1/1 list
    (if (null? plus-minus)
        0
        (+ (* (car plus-minus) (car weights))
           ; multiply next element by its weigth, and add result 
           ; to weighted sum of the rest of the list
           (weighted-vote-sum (cdr plus-minus) (cdr weights)))))
  
  (let ((votes (calc-votes funcs history)))
    ; calculate each function's vote based on history
    (let ((plus-minus (to-plus-minus votes)))
      ; convert votes to plus/minus list
      (let ((vote-sum (weighted-vote-sum plus-minus weights)))
        ; calculated weighted sum of plus/minus list
        (if (>= vote-sum 0) #t #f)
        ; if weighted sum is > 0, return #t, otherwise return #f
        ))))

(display "Test Case for Question 14") (newline)
(prediction (list always-true always-false guess-last guess-not-last)
            (list 0.9 0.9 0.3 0.7)
            (list #t #f #t #f)) ; should return #f
(prediction (list always-true always-false guess-last guess-not-last)
            (list 0.9 0.1 0.3 0.7)
            (list #t #f #t #f)) ; should return #t
(prediction (list always-true always-false guess-last guess-not-last)
            (list 0.1 0.1 0.3 0.7)
            (list #t #f #t #f)) ; should return #f


;;;;;;;;;;;;;;;;;
; Question 15: update-weights
;;;;;;;;;;;;;;;;; 
    
(define (update-weights funcs weights history actual-choice)
  (define (calc-votes funcs history)
    ; apply each func in funcs to history
    ; and return results in a list
      (map (lambda (f) (f history)) funcs))
  
   (define (to-plus-minus votes)
    ; convert #t to 1 and #f to -1
     (map (lambda (x) (if x 1 -1)) votes))
  
  (define (reweight plus-minus weights actual-choice-number)
    (if (null? plus-minus)
        '()
        (cons (if (= (car plus-minus) actual-choice-number) (car weights) (* 0.3 (car weights)))
              (reweight (cdr plus-minus) (cdr weights) actual-choice-number))))
  (let ((votes (calc-votes funcs history)))
    (let ((plus-minus (to-plus-minus votes)))
      (let ((actual-choice-number (if actual-choice 1 -1)))
        (reweight plus-minus weights actual-choice-number)))))

(display "Test case for Question 15") (newline)
(display (update-weights (list always-true always-false guess-last guess-not-last)
                         (list 0.1 0.2 0.3 0.7)
                         (list #t #f #t #f)
                         #t)) ; should return (0.1 0.06 0.3 0.21)


    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Question 16: run learning game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (different-is-good n)
  (lambda (x)
    ((negation (lastn n)) x)))

(define allfuncs
  (list always-true always-false guess-last guess-not-last different-is-good
        (skip-most-recent always-true) (skip-most-recent always-false)
        (skip-most-recent guess-last) (skip-most-recent guess-not-last)
        (skip-most-recent (skip-most-recent always-true)) 
        (skip-most-recent (skip-most-recent always-false))
        (skip-most-recent (skip-most-recent guess-last)) 
        (skip-most-recent (skip-most-recent guess-not-last))
        (lastn 3) (lastn 5) (lastn 7) 
        (negation (lastn 3)) (negation (lastn 5)) (negation (lastn 7))))   
    
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Question 17: run batch-learning game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(batch-learning-game (generate always-true 100)
                     allfuncs)  ; YOUR FINAL SCORE: 0
(batch-learning-game (generate always-false 100)
                     allfuncs) ; YOUR FINAL SCORE: 2
(batch-learning-game (generate guess-not-last 100)
                     allfuncs) ; YOUR FINAL SCORE: 1
(batch-learning-game (generate (skip-most-recent guess-not-last) 100)
                     allfuncs) ; YOUR FINAL SCORE: 2
(batch-learning-game (generate (skip-most-recent
                                (skip-most-recent guess-not-last)) 100)
                     allfuncs) ; YOUR FINAL SCORE: 2
(batch-learning-game (generate (random-choice always-true always-false 0.5) 100)
                     allfuncs) ;YOUR FINAL SCORE: 55
(batch-learning-game (generate (random-choice always-true always-false 0.8) 100)
                     allfuncs) ;YOUR FINAL SCORE: 25


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Question 18: create new function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (different-is-good n)
  (lambda (x)
    ((negation (lastn n)) x)))

((different-is-good 3) (list #t #t #f #f))
((lastn 3) (list #t #t #f #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Question 19: Design a winning sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (winning-sequence n) 
  (define (win-seq-helper n weights history)
    (if (= n 0) history
        (let ((pred (not (prediction allfuncs weights history))))
          (let ((weights (update-weights allfuncs weights history pred)))
            (win-seq-helper (- n 1) weights (cons pred history))))))
  (reverse (win-seq-helper n (list 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) '())))

(display "Test Question 19. My final score should be 100.") (newline)
(batch-learning-game (winning-sequence 100) ; my final score should be 100! yes!
                     allfuncs)
  

                  








          








        





