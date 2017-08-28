#lang racket
;; The built-in procedures car, cdr, and cons make the process of implementing procedures that
;; recursively process and/or build lists quite natural

;; For example, here is a recursive procedure that takes in a list of numbers, and returns
;; the sum of those numbers

(define sum-list
  (lambda (l)
    (if (null? l)
        0
        (+ (car l) (sum-list (cdr l))))))

;; We will trace through the execution of this function on the board

(sum-list '(2 4 6 8 10)); => 30
(sum-list '(1 2 3 4 5)); => 15

;; Here is a function that takes in a list of numbers, and returns the list constructed
;; from multiplying each of those numbers by 2, effectively doubling every element in the
;; original list

(define double-list
  (lambda (l)
    (if (null? l)
        '()
        (cons (* 2 (car l)) (double-list (cdr l))))))

;; We will trace through the execution o this function on the board

(double-list '(1 2 3 4 5)); => '(2 4 6 8 10)
(double-list '(-10 -20 -30)); => '(-20 -40 -60)

;; TASK (define ...) a procedure called 'evens' that takes in a list of integer numbers
;; and returns a new list containing only those numbers from the original list that are even
;; HINT: Scheme provides the built-in procedure even? that checks if a number is even
;; HINT: You will need to use recursion in the implementation of the 'evens' procedure
;;; e.g. the 'evens' procedure must call itself

;; TO BE COMPLETED AND TESTED BY YOU
(define evens
  (lambda (l)
    ...))

;; TEST CASES

(evens '(1 2 3 4 5)); => '(2 4)
(evens '(3 6 9 12 15 18)); => '(6 12 18)
(evens '(1 3 5 7 9)); => '()

;;; SUB-TOPIC: The (map ...) procedure

;; Most scheme implementations come with a built-in procedure called 'map'
;; The map procedure takes in two arguments:
;; 1. A procedure or lambda expression
;; 2. A list of values to apply that lambda expression to, one value at a time
;; The map procedure returns a new list with elements that are the results of calling the procedure
;; on each element of the original list

;; EXAMPLE: Calling map on a lambda expression that adds one to a number, and a list of numbers,
;; returns a list of each of those numbers with one added to it

(map (lambda (x) (+ x 1)) '(1 2 3 4 5)); => '(2 3 4 5 6)

;; The procedure passed as the first arugment to map can either be a predefined procedure with a name
;; or an unnamed "anonymous" procedure (a.k.a a lambda expresssion

;; TASK: Redefine the (double-list ...) procedure that was introduced earlier to use map instead
;; of direct recursion

;; TO BE COMPLETED BY YOU
(define double-list-map
  (lambda (lst) ...))

;; TEST CASES

(double-list-map '(1 2 3)); => '(2 4 6)
(double-list-map '(10 9 8 7)); => '(20 18 16 14)
(double-list-map (double-list-map '(1 2 3 4 5))); => '(4 8 12 16 20)



          
		
