#lang racket
;; A closure is an object consisting of two fields
;; Field 1: A procedure or function that can be called in order to produce a result
;; Field 2: An environment of symbol/value bindings representing the environment that was in
;; effect at the time that the closure object was created.
;; This environment maps each "free variable" in the function definition (each variable that is
;; not a "formal parameter" of the function) to the value that it was associated with at the
;; time the closure was created

;; EXAMPLE

(define b 3)

(define add-3
  (lambda (n)
    (+ n b)))

(add-3 5); => 8
(add-3 10); => 13

;; Because the add-3 procedure that we have defined not only maintains knowledge of the expression(s)
;; that make up its body, but also symbol/value bindings that were in effect at the time it was created
;; This procedure acts as what we call a closure object

;; The add-3 procedure maintains a pointer to an environment, in this case an environment that holds
;; a binding for the free variable 'b'
;; Since we defined 'b' to be the value 3, the environment associated with the add-3 closure has as
;; a record the following binding [b : 3]

;; Therefore, when we call (add-3 5), the closure looks up the value of b in its environment field
;; finds the value associated with it, and replaces the symbol 'b' with the value 3 in the expression
;; (+ n b), so that the expression becomes (+ 5 3), and we then evaluate this expression to get 8

;; Scheme provides another way of binding values to symbols besides using the define keyword
;; The (let ...) syntactic form allows us to specify multiple key-value bindings using a concise
;; notation

;; EXAMPLE

(let ((a 2) (b 3))
  (+ a b)); => 5

;; You can think of the let construct as being a procedure that takes in a list of pairs
;; ((var1 val1) (var2 val2) ...) and creates bindings in the current environment
;; [var1 : val1, var2 : val2, ...] such that var1 and var2 refer to val1 and val2 in any subsequent
;; expressions

;; Using the let construct, we can redefine the add-3 procedure as follows

(define add-3-let
  (let ((a 3))
    (lambda (n) (+ a n))))

;; We have defined add-3-let as being the "closure object" that results from let binding the symbol 'a
;; to the value 3, and creating a lambda expression that takes in a number 'n' and returns (+ a n)

;; This notion of a closure, a function that can maintain knowledge of key-value bindings, is
;; extremely powerful
;; The use of closures is one way that we can model and create abstract objects that maintain
;; local state

;; For instance, we can define a procedure called make-adder, that takes in a number n, and returns
;; a "closure object", a procedure that takes in any number m and returns n + m
;; Effectively the make-adder procedure acts as a "function factory", a function that can be used
;; to create other functions
;; The "function factory" idiom is a well known design pattern among the functional programming
;; community

(define make-adder
  (lambda (n)
    (lambda (m) (+ n m))))

(define add-3-factory-made (make-adder 3))
(add-3-factory-made 5); => 8
(add-3-factory-made 10); => 13

;; Once again, add-3-factory-made is a "closure object" that maintains an environment in which the
;; symbol 'n is bound to whatever value was passed to the make-adder factory procedure

;; TASK: Define a "function factory" called make-list-mutator that takes in a lambda expression
;; of one argument (e.g. (lambda (proc) ...)) and returns a "closure object" that takes in a list
;; of numbers, and that returns the list that is the result of calling (map proc list)

;; HINT: Recall that the map procedure takes in two arguments, a procedure, and a list of values
;; to apply that procedure two one at a time, returning a list of those successive results
;; HINT: THe make-list-mutator procedure should return a lambda, and that lambda will be the closure

;; TO BE COMPLETED BY YOU
(define make-list-mutator
  (lambda (proc)
    ...))

;; To test your new function factory, try to use it to create a closure object called list-doubler
;; that takes in a list of numbers and returns the list that is the result of doubling each of those
;; numbers

;; TO BE COMPLETED BY YOU
(define list-doubler (make-list-mutator (lambda (num) ...)))

;; TEST CASES
(list-doubler '(1 2 3 4 5)); => '(2 4 6 8 10)
(list-doubler '(3 6 9 12)); => '(6 12 18 24)
