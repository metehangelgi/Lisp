;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname inclass2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define a 0 )

(= a  5)



(sqr 9)



(define fact(lambda (n)
(if (= n 1)1
    (* n (fact (- n 1))))))

(fact 81)
