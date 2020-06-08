;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname prelecture5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;aşağıda lazım olacak faktoriyel kodu 
(define fact
  (lambda (n)
    (if (= n 1 ) 1
        (* n (fact (- n 1))))))
;aşağıda lazım olacak iterative faktoriyel kodu
(define ifact (lambda (n) (ifact-helper 1 1 n)))

(define ifact-helper (lambda (product count n) (if (> count n) product
(ifact-helper (* product count) (+ count 1) n))))

;fibonacci nasıl yazılır, bunun time big-O ve space big-O su nedir?

(define fib (lambda (n)
(cond ((= 0 n) 0)
      ((= 1 n) 1)
   (else (+ (fib (- n 1))
     (fib (- n 2)))))))
;time(2^n)- exponential
;space(n)-linear

(fib 5)
(fib 4)
(fib 10)




(define my-expt
        (lambda (a b)
             (if (= b 0) 1
                 (* a (my-expt a (- b 1)))
                 )
          )
  )



(my-expt 5 2)

;space(n)- linear
;time(n)- linear






(define exp-i-help
  (lambda (prod count a)
    (if (= count 0 )
        prod
        (exp-i-help (* prod a) (- count 1) a ))
    )
  )

(define exp-i (lambda ( a b ) (exp-i-help 1 b a ) ))


(exp-i 5 2)

;space(1)- constant
;time(n)-linear




(define fast-exp-1
  (lambda ( a b )
   (cond ((= b 1) a)
         (( even? b) (fast-exp-1 (* a a ) (/ b 2)))
         (else ( * a (fast-exp-1 a (- b 1))))
         )))

;time(logn)-logarithmic
;space(logn)-logarithmic

(fast-exp-1 5 5)




(define pascal
  (lambda ( j n )
    (cond  ((= j 0) 1)
           ((= j n) 1)
     (else (+ (pascal (- j 1) (- n 1))
              (pascal j (- n 1))))
     )))

(define pascal-1
   (lambda (j n)
(cond ((= j 0) 1)
      ((= j n) 1)
      (else (+ (pascal-1 (- j 1) (- n 1))
               (pascal-1 j (- n 1)))))))

;time(2^n)-exponential
;space(n)-linear



(pascal 5 5 )
;(pascal-1 5 4)--- çok büyük space istiyor o yüzden bulamıyor




(define pascal-2 (lambda (j n)
                   (/ (fact n)
                      (* (fact (- n j)) (fact j)
                         ))))

;time(3n)-linear
;space(n)-linear

"pascal-2 denendi hata verdi"
;(pascal-2 5 4)--- kod çalışması gerekiyo fakat bir sorun var
;sonra konuyu öğrenince bakarsın hafızayı aşıyor




;üsttekinden tek farkı iterative faktoriyel kullanması 
(define pascal-3 (lambda (j n)
                   (/ (ifact n)
                      (* (ifact (- n j)) (ifact j) ))))
"pascal-3 pascal-2 ile aynı o yüzden test kodu yok"
;time(n)-linear
;space(1)-constant


;so in direct way--- it is most used way of doing pascal-
;pascal-3 kadar bu da kullanılıyor pascalı bulmak için


""
"kendi yazdığım kod yorum satırında..."
#|
(define pascal-4
  (lambda (j n)
    (/ (help-pascal n 1 (+ n (- j) 1))
       (help-pascal j 1 1))))

(define help-pascal (lambda (k prod end)
                      (if (= k end )
                          (* k prod)
                         (help-pascal (- k 1) (* prod k) end))))
|#

"copy paste kodu benim kod ile aynı sonucu verdi"
;kod hatalı olabilir direk 0 veriyor 
(define pascal-4
    (lambda (j n)
        (/ (help n 1 (+ n (- j) 1))
           (help j 1 1))))
(define help
   (lambda (k prod end)
     (if (= k end)
         (* k prod)
         (help (- k 1) (* prod k) end))))
;time(n)-linear
;space(1)-constant



""
"pascal-4:"
(pascal-4 5 3)
