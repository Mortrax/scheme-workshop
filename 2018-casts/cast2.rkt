#lang racket

; exercises, review from Screen Cast 1

; Question 1 (*): Review on simple data types
; Complete each of the following expressions so
; that they all evaluate to true by providing
; an argument of the appropriate type to each
; of these type predicates
(symbol? )
(number? )
(string? )
(list? )
(null? )

; Question 2 (*): Review on list constructions

(define a (list 1 2 3 4 5))

; The above list is constructed using the list procedure

; Construct equivalent lists using three other methods

; Using only calls to the cons procedure
(define b ...)

; Using a dotted pair literal
(define c ...)

; By providing two arguments to the call to (append)
(define d (append ...))

; Complete the above expressions so that the following
; expression evaluates to #t


(equal-lists? a b c d)

(define (equal-lists? l1 l2 l3 l4)
  (and (equal? l1 l2) (equal? l1 l3) (equal? l1 l4)))

; (procedures, lambdas, and conditionals)

; method

; Lambda calculus
; a x b

; (lambda (x) (+ x x))

; ((lambda (x) (+ x x)) 2) => 4

; (add-two 5 7)

; Environment

; {
;  add-two: (lambda (x y) (+ x y))
;  +: #<procedure>
;  *: #<procedure>
;  cons: #<procedure>
; ....
; }

; (add-two 5 7)
; ((lambda (x y) (+ x y)) 5 7)
; (+ 5 7)
; 12

; Higher-order functions

; map: (a -> b) -> a List -> b List
; f(x) [x, x, x, x, ...] => [f(x), f(x), f(x), f(x), ...]

; Closure

(define make-adder (lambda (n)
                     (lambda (x) (+ n x))))

(define add10 (make-adder 10))


; (define add10 (make-adder 10))
; (define add10 ((lambda (n) (lambda (x) (+ n x))) 10))
; (define add10 (lambda (x) (+ 10 x)))
; {
;  add10: (lambda (x) (+ 10 x))
; }

; (add10 5)
; ((lambda (x) (+ 10 x)) 5)
; (+ 10 5)
; 15

; {
;  add10: (lambda (x) (+ n x))
; }

; Frame
; child  parent
; {   -> {
;          make-adder: (lambda ...)
;  n: 10   add10: (lambda (x) (+ x n))
; }      }

; (lambda (x) (+ x n))
; pointer: -> child





