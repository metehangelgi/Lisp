; load project code
(load "rsa.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 1: Addition, Subtraction, Multiplication, modulo n 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define +mod
  ; input: integers a, b, n
  ; output: a + b (mod n)
  (lambda (a b n)
    (modulo (+ a b) n)))

(define -mod
  ; input: integers a, b, n
  ; output: a - b (mod n)
  (lambda (a b n)
    (modulo (- a b) n)))

(define *mod
  ; input: integers a, b, n
  ; output: a * b (mod n)
  (lambda (a b n)
    (modulo (* a b) n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 2: Raising a Number to a Power 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define slow-exptmod
  ; input: integers a, b, n
  ; output: a^b (mod n)
  (lambda (a b n)
    (if (= b 0)
        1
        (*mod a (slow-exptmod a (- b 1) n) n))))

; order of growth of slow-exptmod in time is O(n)
; order of growth in space is also O(n)
; algorithm is recursive

(define exptmod
  ; input: integers a, b, n
  ; output: a^b (mod n)
  (lambda (a b n)
    (cond ((= b 0) 1)
          ((= 0 (remainder b 2)) ((lambda (x)
                                    (*mod x x n))
                                  (exptmod a (/ b 2) n)))
          (else (*mod a (exptmod a (- b 1) n) n)))))

; test cases for exptmod
;> (exptmod 2 0 10)
;1
;> (exptmod 2 3 10)
;8
;> (exptmod 3 4 10)
;1
;> (exptmod 2 15 100)
;68
;> (exptmod -5 3 100)
;75
;>

; order of growth of exptmod in time is O(log n)
; order of growth of exptmod in space is O(log n)
; algorithm is recursive

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 3: Large Random Numbers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define random-k-digit-number
  ; input: integer k
  ; output: random number with k digits
  (lambda (k)
    (define random-k-digit-helper
      (lambda (n)
        (if (= n 1)
            (random 10)
            (+ (* (expt 10 (- n 1)) (random 10))
               (random-k-digit-helper (- n 1))))))
    (random-k-digit-helper k)))

; test cases for random-k-digit-number
;> (random-k-digit-number 1)
;2
;> (random-k-digit-number 3)
;299
;> (random-k-digit-number 3)
;328
;> (random-k-digit-number 50)
;65184702519528762834751385913115357584423922061028

(define count-digits
  ; input: integer n
  ; output: number of digits in the decimal representation of n
  (lambda (n)
    (if (< n 10)
        1
        (+ 1 (count-digits (/ n 10))))))

; test cases for count-digits
;> (count-digits 123)
;3
;> (count-digits 123456789)
;9
;> (count-digits 1)
;1

(define big-random
  ; input: integer n
  ; output: random integer < n
  (lambda (n)
    (let ((digits (count-digits n)))
      (let ((rand-num (random-k-digit-number digits)))
        (if (< rand-num n)
            rand-num
            (big-random n))))))

;test cases for big-random
;> (big-random 100)  1-2 digit random number
;56
;> (big-random 100)  is it different?
;13
;> (big-random 1)    should always be 0
;0
;> (big-random 1)    should always be 0
;0
;> (big-random (expt 10 40))    roughly 40 digit number
;9664183975395174228637980215867841309457
;> (count-digits (big-random (expt 10 40)))
;40


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 4: Prime Numbers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test-factors
  (lambda (n k)
    (cond ((>= k n) #t)
          ((= (remainder n k) 0) #f)
          (else (test-factors n (+ k 1))))))

(define slow-prime?
  (lambda (n)
    (if (< n 2)
        #f
        (test-factors n 2))))

; order of growth of test-factors:
; in time: O(n)
; in space: O(1)
; test-factors is iterative

; orders of growth of test-factors with Ben Bitdiddle's suggestions:
; first suggestion:
; in time: O(sqrt n)
; in space: O(1)
; second suggestion:
; in time: O(n)
; in space: O(1)

(define prime?
  ; input: integer p
  ; output: boolean value #t if p is prime, #f otherwise
  (lambda (p)
    (if (< p 2) 
        #f
        (let ((prime-test-iterations 20))
          (define prime?-helper
            (lambda (p prime-test-iterations)
              (let ((a (big-random p)))
                (let ((a-expt-p (exptmod a p p)))
                  (cond ((= prime-test-iterations 0) #t)
                        ((= a-expt-p a) (prime?-helper p (- prime-test-iterations 1)))
                        (else #f))))))
          (prime?-helper p prime-test-iterations)))))
      
                         
;test cases for prime?:
;> (prime? 2)
;#t
;> (prime? 4)
;#f
;> (prime? 1)
;#f
;> (prime? 0)
;#f
;> (prime? 200)
;#f
;> (prime? 199)
;#t
;> (prime? 17)
;#t

; order of growth of prime?
; in time and space: O(log p) because it calls exptmod a constant number of times, and exptmod is O(log p) 
; prime? is iterative


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 5: Random Primes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define random-prime 
  ; input: integer n
  ; output: prime number < n
  (lambda (n)
    (let ((p (big-random n)))
      (if (prime? p)
          p
          (random-prime n)))))

; random-prime could fail if prime? returns a composite number. 
; This could happen if big-random returns a Carmichael number, a 
; composite number that passes the Fermat test.
; Fortunately, as n -> Inf, Carmichael numbers become rare. 
; From Wikipedia: there are 20,138,200 Carmichael numbers between 
; 1 and 10^21 (approximately one in 50 trillion (50e12) numbers)
    
;test cases for random-prime
;> (random-prime 3)
;2
;> (random-prime 3)      must be 2
;2
;> (random-prime 100)
;37
;> (random-prime 100)    is it different?
;67
;> (random-prime 100000)
;8293



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 6: Multiplicative Inverses
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ax+by=1
  ; input: integers a, b > 0
  ; output: integers x, y s.t. ax + by = 1
  (lambda (a b)
    (let ((q (quotient a b))
          (r (remainder a b)))
      (if (= r 1)
          (cons 1 (- q))
          (let ((ans (ax+by=1 b r)))
            (let ((x (car ans))
                  (y (cdr ans)))
              (cons y (- x (* q y)))))))))

;test cases for ax+by=1
;> (ax+by=1 17 13)
;(-3 . 4)
;> (ax+by=1 7 3)
;(1 . -2)
;> (ax+by=1 10 27)
;(-8 . 3)                  

(define inverse-mod
  (lambda (e n)
    (if (not (= (gcd e n) 1))
        (error "gcd(e,n) must equal 1")
        (let ((ans (car (ax+by=1 e n))))
          (if (> ans 0)
              ans
              (+ ans n))))))
          
;test cases for inverse-mod
;> (inverse-mod 5 11)
;9
;> (inverse-mod 9 11)
;5
;> (inverse-mod 7 11)
;8
;> (inverse-mod 5 12)
;5
;> (inverse-mod 8 12)
; gcd(e,n) must equal 1
;> (inverse-mod (random-prime 101) 101)  test this answer with *mod
;71
;> (*mod (inverse-mod 71 101) 71 101)
;1


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 7: RSA
;;
;; Converting message strings to and from
;; integers.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-key 
  ; input: exponent e (integer) and modulus n (integer)
  ; output: (e, n) pair
  (lambda (e n)
    (cons e n)))

(define get-modulus
  ; input: key
  ; output: modulus of key
  (lambda (key)
    (cdr key)))

(define get-exponent
  ; input: key
  ; output: exponent of key
  (lambda (key)
    (car key)))

(define random-keypair
  (lambda (m)
    
    (define get-primes
      ; input: modulus m
      ; output: primes p, q < m s.t. pq > m
      (lambda (m)
          (let ((p (random-prime m))
                (q (random-prime m)))
            (if (> (* p q) m)
                (cons p q)
                (get-primes m)))))
    
    (define get-mod
      ; input: pair of primes p, q
      ; output: modulus = p*q
      (lambda (primes)
        (* (car primes) (cdr primes))))
    
    (define get-e
      (lambda (primes mod)
        (let ((e (big-random mod))
              (p-1 (- (car primes) 1))
              (q-1 (- (cdr primes) 1)))
          (if (= (gcd e (* p-1 q-1)) 1)
              e
              (get-e primes mod)))))
    
    (define get-d 
      (lambda (e primes)
        (let ((p-1 (- (car primes) 1)) 
              (q-1 (- (cdr primes) 1)))
          (inverse-mod e (* p-1 q-1)))))
    
    (let ((primes (get-primes m)))
      (let ((m (get-mod primes)))
        (let ((e (get-e primes m)))
          (let ((d (get-d e primes)))
            (cons (make-key e m) (make-key d m))))))))
      
(define rsa
  (lambda (key message)
    (let ((e (get-exponent key))
          (m (get-modulus key)))
      (exptmod message e m))))

(define encrypt
  (lambda (public-key string)
    (let ((message-number (string->integer string)))
      (rsa public-key message-number))))

(define decrypt
  (lambda (private-key encrypted-message)
    (let ((message-number (rsa private-key encrypted-message)))
      (integer->string message-number))))

;test cases for random-keypair, encrypt, and decrypt:
;> (define keys (random-keypair (string->integer "hello, world!")))
;> (define encrypted (encrypt (car keys) "hello, world!"))
;> (define decrypted (decrypt (cdr keys) encrypted))
;> decrypted
;"hello, world!"
;> (define keys (random-keypair (string->integer "meet me in 6.001 lab")))
;> (define encrypted (encrypt (car keys) "meet me in 6.001 lab"))
;> (define decrypted (decrypt (cdr keys) encrypted))
;> decrypted
;"meet me in 6.001 lab"                     