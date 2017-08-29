#lang racket

;; MACROS
;; The Scheme programming language provides sophisticated functionality that allows programmers
;; to extend the core syntactic constructs of the language with their own syntax

;; The general means by which this is accomplished is via a construct known as a macro

;; A "macro" is a syntactic form, along with an associated "transformer" that can "expand" the
;; syntactic form of the macro into existing, built-in forms in the language

;; The R7RS standard of scheme provides the built-in 'define-syntax' from along with the 'syntax-rules'
;; "transformer" form

;; The usage of both of these forms can be described as follows

;; (define-syntax <id>
;;   (syntax-rules (<literal-id> ...)
;;     [pattern template]
;;     ...))

;; In our earlier modules, we used the built in (lambda (<args> ...) <body>...) form to
;; define and create new procedure objects from other built in procedures in the language
;; We also saw that we can use the (let ((<var1> <val1>) ...) <body>...) to create new local bindings
;; that will be available in the <body>... expressions following the let bindings

;; THEOREM
;; Binding variables to values with a let, and then evaluating expressions that rely on those bindings
;; is equivalent to writing a lambda expression, with the variables in the formal parameters list, the
;; expressions in the body expression of the lambda, and then calling that lambda on the values

;; (let ((var val) ...) e1 e2 ...) is equivalent to
;; ((lambda (var ...) e1 e2)) val ...)

;; EXAMPLE: We can define (let ((<var1> <val1>) ...) <body> ...) in terms of lambda using the built-in
;; macro functionality of Scheme. We will do this now

(define-syntax my-let
  (syntax-rules ()
    [(my-let ((x v) ...) e1 e2 ...)
     ((lambda (x ...) e1 e2 ...) v ...)]))

;; Essentially, what we have done is define a new syntactic form called 'my-let' that when invoked on
;; a scheme expression, expands to a new expression involving the call of a lambda on a set of values

;; EXAMPLE

(my-let ((a 1) (b 2)) (+ a b)); => 3
;; Expands to
((lambda (a b) (+ a b)) 1 2); => 3

;; Each unbound identifier in the macro pattern (my-let ((x v) ...) e1 e2 ...) must correspond either
;; to an identifier in the environment, or to an unbound identifier in the associated expanded form
;; An identifier, followed by an ellipsis, functions akin to a star pattern, matching 0 or more
;; identifiers
;; Two identifiers, followed by an ellipsis, functions akin to a plus pattern, matching 1 or more
;; identifier

;; TASK: Define a new syntactic form called 'when' that takes in a predicate and
;; one or more expressions, evaluating those expressions in succession when the predicate is true
;; If the predicate is false, simply return #f for false
;; HINT: Use the define-syntax and syntax-rules constructs introduced above
;; - The expanded form of the macro will need to make use of the built in (if ...) and (begin ...)
;; constructs
;; - Think about what identifiers and patterns you will need to match on in the pattern expression
;; when defining your macro

;; To be completed by you

(define-syntax when
  (syntax-rules ()
    ...))

;; TEST CASES

(when (> 5 2) (display 'Success) (newline) (+ 5 20))
; => Success
; => 25

(when (< 5 -5) (+ 20 30)); => #f

;; TASK: Define a new syntactic form called 'unless' that takes in a predicate and one or more
;; expressions, evaluating those expressions in succession when the predicate is false
;; If the predicate is true, simply return #f for false
;; HINT: This syntactic form should be equivalent to (when (not test-expression) body ...)

;; To be completed by you

(define-syntax unless
  (syntax-rules ()
    ...))

;; TEST CASES

(unless (< 5 2) (display 'Success) (newline) (+ 4 20))
; => Success
; => 24

(unless (= 1 1) (+ 20 20)); => #f