;; In most mainstream programming languages such as C/C++/Java/Python etc. If you want
;; to execute the same code multiple times in a row, you use some sort of looping construct
;; Examples of these would be a while loop, which loops while a certain condition is true
;; or a for loop, which loops a set number of times

;; In functional languages like Scheme, the primary means by which we execute code
;; multiple times is via a technique known as recursion. Recursion occurs when something
;; is defined in terms of itself
;; A recursive algorithm is an algorithm that employs recursion to do its work
;; A recursive function, or recursive procedure, is a function that is defined in terms
;; of itself, in order to be able to carry out a recursive algorithm

;; In scheme, we can defined recursive procedures quite elegantly and simply using the same
;; define and lambda built-ins that we did before

;; What follows is a recursive definition of a Scheme procedure that computes the factorial
;; of a number
;; For any given natural number input n (recall that natural numbers are 0, 1, 2, 3, 4, 5...etc);; This procedure computes n!, or "n factorial".

;; The factorial of a number is defined recursively as
;;; 0! = 1
;;; n! = n * (n - 1)!
;;; By this recursive definition, we compute 3! as follows
;;;; 3! = 3 * (3 - 1)! => 3 * 2! => 3 * 2 * (2 - 1)! => 3 * 2 * 1! => 3 * 2 * 1 * (1 - 1)!
;;;; => 3 * 2 * 1 * 0! => 3 * 2 * 1 * 1 => 6

;; Here is how we define the factorial function in Scheme

(define factorial
	(lambda (n)
		(if (zero? n)
			1
			(* n (factorial (- n 1))))))

;; Factorial is a procedure that takes in one argument, the natural number 'n' for which we
;; want to compute n!

;; Factorial then checks if n is zero (if (zero? n) ...)
;; If n is indeed 0, we return 1
;; If n is not 0, we instead return the result of multiplying n, by a recursive call to 
;; factorial on n - 1 (factorial (- n 1))

;; Test the factorial funciton in your REPL now

(factorial 5); => 120
(factorial 3); => 6
(factorial 4); => 24

;; etc.
