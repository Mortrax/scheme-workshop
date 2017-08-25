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

;;;; Try typing the following code into your Guile REPL

(define a 1); The symbol a now refers to the number 1 in the current environment
(define b 2); The symbol b now refers to the number 2 in the current environment

(+ a b); => 3

(define a-squared (* a a)); => 1

(define b-squared (* b b)); => 4
