;; Scheme is a dialect of the Lisp programming language
;; The name LISP originally stood for LIStProcessing, because LISP was originally
;; designed as a list based programming language
;; For this reason, in Scheme, as in most Lisp dialects, the canonical data structure is
;; the list, which represents a traditional linked-list

;; In scheme, lists can be created directly with the built in (list args....) procedure

(list 1 2 3 4); Creates the list '(1 2 3 4)

(list 'a 'b 'c 'd); Creates the list of symbols '(a b c d)

;; In scheme, lists are actually a special case of the more general lisp data structure known
;; as a pair. In scheme, pairs are denoted as a dotted pair of values surrounded by
;; parentheses
;; For example, we could construct the pair of numbers 1 and 2 as '(1 . 2)
;; The pair of symbols a and b could be constructed as '(a . b)

;; In scheme, the built-in 'cons' (for construct) is used for creating or "constructing" pairs

(cons 1 2); => '(1 . 2) a pair of numbers
(cons 'a 'b); => '(a . b) a pair of symbols
(cons (list 1 2 3) (list 4 5 6)); => '((1 2 3) . (4 5 6)) a pair of lists
(cons (cons 1 2) (cons 3 4)); => '((1 . 2) . (3 . 4)) a pair of pairs

;; In order to access the first element of a pair, we use the built-in procedure 'car'
;; In order to access the second element of a pair, we use the built-in procedure 'cdr'

(define pair (cons 1 2)) ;; Now we have the entry [pair : '(1 . 2)] in the environment

(car pair); => 1
(cdr pair); => 2

;; As mentioned previously, lists are just a special instance of a pair, in which the car
;; of the list is the first element in the list, and the cdr of the list is all of the elements
;; in the list with the first element removed

(define l (list 1 2 3 4 5))

(car l); => 1
(cdr l); => '(2 3 4 5)
l; > '(1 2 3 4 5)

;; The reason this works, is because the call to (list 1 2 3 4 5), is equivalent to the
;; following nested call to cons

(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '()))))); => '(1 2 3 4 5)

;; In scheme, our way of representing lists as parenthesized values separated by spaces
;; e.g. '(1 2 3 4 5)
;; is actually just syntactic sugar for the more precise dotted pair notation
;; e.g. '(1 . (2 . (3 . (4 . (5 . ())))))
;; To re-iterate this concept, try constructing this list in the DrRacket REPL using the
;; dotted pair convention, you will see that it evaluates to the same list

'(1 . (2 . (3 . (4 . (5 . ()))))); => '(1 2 3 4 5)

;; To summarize, a properly formed list in Scheme is simply a pair, whose car can be any
;; Scheme datum, but whose cdr must be another list
;; The nth-cdr of a list of n elements must always be the empty list, denoted as '()


(define new-list '(a b c d e f))

;; an equivalent way to construct this list is (list 'a 'b 'c 'd 'e 'f)

(car new_list); => 'a
(cdr new_list; => '(b c d e f)
(cdr (cdr new_list)); => '(c d e f)
(cdr (cdr (cdr new_list))); => '(d e f)
