#lang racket
;; In Scheme, symbols are used as variable names, referring to data objects
;; or values. This is distinct from the C/C++/Java notion of a variable.
;; In languages like C/C++, variables refer to locations in memory, that may
;; or may not hold relevant data. Scheme variables are not treated the same
;; way conceptually

;; Variable/value bindings are recorded in a construct known as the Environment
;; which we will elaborate on later in the workshop
;; For now, it will suffice to think of the environment as a data table that
;; keeps track of variable names and their associated values

;;; In scheme, we can form a new variable/value binding with the built in
;;; define construct as so: (define <variable-name> <value>)
;;; where <variable-name> is a symbol that is unbound to any other value, and
;;; value is a data object e.g. number, list, procedure/closure, etc.

;;;; Try typing the following code into your Dr. Racket REPL

(define a 1); The symbol a now refers to the number 1 in the current environment
(define b 2); The symbol b now refers to the number 2 in the current environment

(+ a b); => 3

;;;; We can also set symbols to refer to the values returned by more complex expressions

(define a-squared (* a a)); => 1

(define b-squared (* b b)); => 4

;;;; You can be as complex as you want in your assignments
;;;; As long as the second argument to (define...) is a valid s-expression
;;;; (define...) will successfully bind its first argument to the value of its
;;;; second argument in the environment

(define a-squared-minus-b-squared (- a-squared b-squared));

;; TASK: Define symbol x to be the number 5
;; - Define symbol y to be the number 10
;; - Define symbol z to be twice x, plus y

;; To be Completed by You

(define x ...)
(define y ...)
(define z ...)

x ;=> 5
y ;=> 10
z ;=> 20


