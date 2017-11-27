#lang racket
(require srfi/1)
(define chinese '("ling" "yi" "er" "san" "si" "wu" "liu" "qi" "ba" "jiu" "shi"))
(define english '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"))
(define numbers '(0 1 2 3 4 5 6 7 8 9))
(define test '("yi" "er" "two"))
(define test2 '("yi" "josh" "three" "si"))


; method prints all the information and discards invalid values.
(define (go alist)(
       display "The list is: ")
       (define other (filter (lambda (x) (>= x 0)) (mapper translate alist)))
       (display other )
       (display "\n")
       (display (add-between other "+"))
         (display " = ")
      (display (adder  other ))
         (display "\n")
       (display (add-between  other  "*"))
         (display " = ")
      (display (multiplier  other )))         
       

; translates word into corresponding number. if the word is "invalid" method will translate it into negative number
(define (translate thing)(
  + (if(eq? (boolean? (list-index (curry equal? thing) chinese)) #f) (list-index (curry equal? thing) chinese) -1) (if(eq? (boolean? (list-index (curry equal? thing) english)) #f) (list-index (curry equal? thing) english) -1)  1 ))

;adds all numbers in the list
(define (adder alist)
  (if
    (null? alist)
    0
    (+ (car alist) (adder (cdr alist)))
  )
)

; multiplies numbers inthe list
(define (multiplier alist)
  (if
    (null? alist)
    1
    (* (car alist) (multiplier (cdr alist)))
  )
)

;method maps list of words into list of integers
(define (mapper trans l)
   (if (null? l) '()
       (if (list? (car l))
           (cons (mapper trans (car l)) (mapper trans (cdr l)))
           (cons (trans (car l)) (mapper trans (cdr l))))))