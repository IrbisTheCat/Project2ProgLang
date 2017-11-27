#lang racket
;question 1
(define first (lambda (x) (car x)))
(define second (lambda (x) (cadr x)))
(define third (lambda (x) (caddr x)))
(define foruth (lambda (x) (cadddr x)))
(define fifth (lambda (x) (car(cdr(cdr(cdr(cdr x)))))))
(define rest (lambda (x) (cdr x)))

;question 2
(define mylist(list #t #f #t #f #t))
(define (is-true x) (eq? x #t))
(define (truecount alist) (length(filter(lambda(x)(is-true x))alist)))

;question 3
(define mytoo(list 1 2 3 4 5))
(define (squarelist alist)
(map (lambda (x) (* x x)) alist))

;question 4
(define mythree(list 1 200 3 4 500))
(define (bigger x) (> x 100))
(define (hundreds alist)
 (filter(lambda(x)(bigger x))alist))


;question 5
(define (collatz n)(
  helper n)                
 )
  
(define (helper n)(
   cond
   ((= 1 n)(display n))
   ((odd? n)(display n) (display " ") (helper (+ (* n 3) 1)))
   ((even? n) (display n) (display " ") (helper(/ n 2)))
   (else list)
   ) )
  
                    
