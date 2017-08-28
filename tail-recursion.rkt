#lang racket
;; The R7RS standard (along with most Scheme standards) stipulates that all Scheme implementations
;; intending to conform to this standard perform what is known as tail-call-optimization

;; In order for tail-call-optimization to be applied, programs, algorithms, and procedures must be
;; written such that they employ a special flavor of recursion known as "tail-recursion"


;; A "tail-call" is a procedure call (i.e. (+ 1 2)) that is performed as the final action of
;; a procedure

;; A tail recursive procedure is a recursive procedure, in which the recursive call is performed as
;; the final action of that procedure during the recursive case

;; To illustrate this, we will return to the factorial example introduced in our earlier module
;; on recursion

(define factorial
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))

;; This procedure implements a recursive algorithm for finding the factorial n! of any natural number n
;; However, there is a subtle problem in the way in which this procedure is implemented, it is not
;; tail recursive

;; What this means is that, in the recursive case, the recursive call to (factorial (- n 1)) is NOT
;; the final action of the factorial procedure. The final action is in fact the action of multiplying
;; the current value of n, to the value returned by the recursive call to factorial

;; Let's trace through an example call of our factorial function on the number 4

;; (factorial 4)
;; => (* 4 (factorial 3))
;; => (* 4 (* 3 (factorial 2)))
;; => (* 4 (* 3 (* 2 (factorial 1))))
;; => (* 4 (* 3 (* 2 (* 1 (factorial 0)))))
;; => (* 4 (* 3 (* 2 (* 1 1))))
;; => (* 4 (* 3 (* 2 1)))
;; => (* 4 (* 3 2))
;; => (* 4 6)
;; => 24

;; As you can see, the factorial procedure as it is currently implemented needs to expand its
;; expression size in order to save all of the multiplications that will need to be evaluated once
;; we reach the base case of our algorithm (e.g. once n becomes 0)
;; While this is not necessarily inefficient in terms of computation time, it is inefficient in terms
;; of stack or heap memory needed to accomodate all of the recursive calls to factorial

;; In order to optimize away the need to save all of the prior stack frames created for each recursive
;; call to factorial, we need to modify our factorial procedure so that the recursive call is the
;; final action performed in the recursive case

;; We need to convert the factorial procedure into one which uses a style known as
;; Accumulator passing style
;; In accumulator passing style, if there is a value that we are building up in the recursive case
;; of a procedure, instead of building up this value outside of the recursive call, we can instead
;; build up and maintain this value in an extra parameter passed to the recursive procedure
;; This extra parameter is colloquially known as the "accumulator", hence the name accumulator
;; passing style

;; Below, we shall walk through re-defining the factorial procedure as a tail-recursive procedure

(define tail-factorial
  (lambda (n acc)
    (if (zero? n)
        acc
        (tail-factorial (- n 1) (* n acc)))))

;; What we have done is define a new procedure called tail-factorial, that takes in a natural number n
;; for which we want to compute n! (n factorial) and an extra accumulator parameter that will
;; be used to build up our result
;; In the base case, the case in which n = 0, we return this accumulator, which should hold the value
;; of n!
;; In the recursive case, we simply call tail-factorial on n - 1 and the result of multiplying the
;; current value of n by the current value of the accumulator

;; In order to call tail-factorial, we must pass in not only the number n for which we want to compute
;; n!, but also an appropriate accumulator value to hold the result

;; TASK: What should the value of our accumulator be when we first call tail-factorial?
;; Think about what the factorial function should return in the base case (e.g. in the case that n = 0)

;; TO BE COMPLETED BY YOU
;; REPLACE THE _ WITH THE APPROPRIATE BASE ACCUMULATOR VALUE IN THE FOLLOWING EXPRESSIONS

;; TEST CASES
(tail-factorial 5 _); => 120
(tail-factorial 0 _); => 1
(tail-factorial 3 _); => 6
(tail-factorial 4 _); => 24

;; In a production setting, to save the user from having to worry about what parameter to pass to
;; tail-factorial funciton as an accumulator, we could of course wrap this procedure in a 'factorial'
;; procedure that only takes one argument, the number n

(define factorial2
  (lambda (n)
    (tail-factorial n _)))

;; TASK: A tail factorial implementation of sum-list
;; Write a procedure called sum-list that takes in a list of numbers, and returns the sum of those
;; numbers
;; Your procedure must make use of a helper procedure that is written in the accumulator passing style
;; Meaning that the helper procedure must be tail recursive
;; As a reminder, the ordinary recursive sum-list procedure can be written as

(define sum-list-non-tail
  (lambda (lst)
    (if (null? lst)
        0
        (+ (car lst) (sum-list-non-tail (cdr lst))))))

;; HINT: Think about what value the accumulator in your helper procedure will need to hold
;; HINT: You will need to wrap this helper procedure, calling it with the appropriate base
;; - accumulator value in the definition of your sum-list procedure

;; TO BE COMPLETED BY YOU

(define sum-list
  (lambda (lst)
    ...))

(define sum-list-helper
  (lambda (lst acc)
    ...))