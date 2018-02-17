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







