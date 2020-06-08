;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname inclass1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define square  (lambda (x) (* x x)))

(define pyt (lambda(x y) (sqrt(+ (square x) (square y)))))




(define close-enough
  (lambda (guess x)
    (< (abs (- (square guess) x)) 0.001)))

(define average
  (lambda (a b) (/ (+ a b) 2)))




