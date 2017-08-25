;; The simplest Scheme expressions are constant data objects:
;;; EXAMPLES
;;;
;;;; Strings like "hello!" and "Good morning Dave"
;;;; Numbers like 5, 4, 1/2, and 3.14159
;;;; Symbols like thomas, x, peanut, and symbol
;;;; Lists like (1 2 3 4 5), (a b c d e f), etc.

;;;; Try typing the following constant expressions into your Guile REPL

5

'hello

1/2

'(a b c d e)

"The Bird of Hermes is my name."

;; Scheme provides the names +, -, *, and / to represent the corresponding
;; arithmetic procedures that you should all be familiar with from other
;; programming languages
;; These are just symbols that have been pre-bound in the top-level environment
;; to native-procedures that (in our system) are implemented in C++

;;; Scheme employs prefix notation for all procedure applications, even for
;;; the common arithmetic operations listed above. The syntax for such
;;; procedure applications is (<procedure-name> <arg1> ...)
;;; This adds a great deal of regularity to the syntax of the language, since
;;; the same notation is employed regardless of the specific procedures being
;;; applied in an expression
;;;; Try typing the following expressions into your Guile REPL

(+ 2 2); 2 + 2 => 4
(* 5 10); 5 * 10 => 50
(/ 1 2); 1 / 2 = 1/2

;; NOTE: Arithmetic procedures can take an aribtrary number of arguments
;; This is the beauty of prefix notation, we need not type the operator more
;; than a single time :)

(+ 1 2 3 4 5); => 15
(* 1 2 3 4 5); => 5!

;; Procedure applications may be nested arbitrarily, in which case the
;; innermost values will be computed first
;; We can therefore nest applications of arithmetic procedures to evaluate
;; more complicated mathematical formulas

(+ (+ 2 2) (+ 2 2)); => 8
(- 2 (* 4 1/3)); => 2/3
(* 2 (* 2 (* 2 (* 2 2)))); => 32
