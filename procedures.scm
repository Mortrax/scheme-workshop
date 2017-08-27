;; Most Scheme implementations come with a number of pre-defined procedures
;; Examples of procedures that you have seen so far are +, -, *, and /
;; These are symbols that are bound to built in procedures in the top level environment
;; When ever you start up a Scheme REPL (Read Eval Print Loop)

;; Scheme also allows you to define your own procedures
;; This is also done with the built in (define ...) primitive that we used previously
;; Scheme procedures are known as lambda expressions, and scheme uses the lambda keyword to 
;; denote that the corresponding s-expression is a lambda expression a.k.a a "procedure"

;; For instance, if we wanted to define a procedure to square a number, we would do that
;; as follows

(define square (lambda (a) (* a a)))

;; What we have done here, is set the symbol 'square to refer to the lambda expression
;; (lambda (a) (* a a)) in the current environment
;; A lambda expression is a list, where the first element of the list is the symbol 'lambda,
;; the second element is a list of formal parameters (symbols which can be bound to values)
;; and the final element is a list of s-expressions that specify the behavior of the lambda
;; procedure

;; In the case of our square procedure, we can see that it takes in one argument, which is
;; bound to the parameter 'a. This procedure then passes two copies of the value of a to the
;; built in * procedure. Effectively, this function squares a number that it is called on

;; To call a procedure in scheme, we simply type (<procedure-name> arg1 ...)
;; To call our square procedure on the number 2, we write this as follows...

(square 2); => 4

;; Try converting the following expression into a valid scheme expression that uses our
;; newly defined square procedure, and evaluate the expression in your REPL
;; 5^2 + 10^2

;; We can define procedures that take in any number of arguments
;; For an n-ary procedure (a procedure that takes 'n' argumetns) simply provide n-many
;; distinct symbols in the formal parameter list of the lambda declaration

;; Some Examples

;; A procedure that adds three numbers together, a "3-ary procedure"

(define add3 (lambda (a b c) (+ a b c)))

;; calling the add3 procedure

(add3 1 2 3); => 6

;; A procedure that divides its first argument by its second argument, a "2-ary procedure"

(define ratio (lambda (a b) (/ a b)))

;; calling the ratio procedure

(ratio 1 2); => 1/2

;; Procedures can also be defined to take an arbitrary number of arguments
;; Procedures that take a variable number of arguments are known as "variable-arity" procedures

;; Variable-arity procedures can be defined with an input parameter that is a single symbol
;; instead of a list of symbols

;; This is a function that takes in an arbitrary number of arguments, and adds them together
;; returning the sum

(define addn (lambda x (apply + x)))

;; Calling the addn function

(addn 1 2 3 4 5); => 15

;; When addn is called, its arguments get wrapped into a list, in the above case becoming the
;; list '(1 2 3 4 5)
;; The built in 'apply' procedure takes in two arguments, a procedure that can be apllied to
;; arguments, and a list of values to apply that procedure to
;; Essentially, the apply procedure places the plus procedure at the head of the list
;; Transforming '(1 2 3 4 5) to '(+ 1 2 3 4 5)
;; apply then evaluates the list '(+ 1 2 3 4 5), to yield the value 15


