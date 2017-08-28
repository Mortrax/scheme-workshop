;; In many cases, computer programs must respond to different inputs in different ways
;; In a sense, computer programs must be able to make a decision as to how to proceed
;; with the next stage of computation, depending upon if a condition, or set of conditions,
;; has been met

;; For example, an e-mail application may need to decide when it is an appropriate time
;; to display the contents of a specific e-mail to the user, perhaps in response to the
;; user clicking on that e-mail in a GUI

;; An rpg game program may have to decide when it is time to grant the player character a
;; new level, perhaps when that character has acquired a pre-defined number of experience
;; points

;; In order to deal with conditional computation and decision making, we need a way of
;; expressing when a certain condition is met, or when it is not met i.e. Is it true or false
;; that the player character has obtained sufficient xp to level up?

;; In order to handle this notion of truth or falsehood, we use a class of values known as
;; BOOLEAN or truth values. A boolean data object is an object that can only ever have one
;; of two values, truth or false.

;; In Scheme, true is denoted by the symbol #t, and false by the symbol #f

;; This example source will introduce the following concepts
;;; Boolean expressions: expressions that evaluate to a boolean value (e.g. true or false)
;;; Conditional expressions: expressions that execute code and produce a result depending
;;; on the Boolean result of some boolean expression

;; Starting with boolean expressions, Scheme provides the following set of comparison
;; operators for numbers that you may remember from highschool mathematics.
;; Recall that for any two numbers a and b, either a is greater than b, a is equal to b, or
;; a is less than b. This is known as the Mathematical Law of Trichotomy

(> 5 2); => #t, 5 is greater than 2
(< 1 5); => #t, 1 is less than 5
(= 10 10); => #t, 10 is equal to 10
(< 20 20); => #f, 20 is not less than 20
(<= 20 20); => #t; 20 is less than OR equal to 20

;; Continuing on to conditional expressions
;; The way that scheme handles conditional computation is via a built in syntactic form
;; known as 'if'

;; An if statement is constructed as follows: 
;;; (if (<conditional>)
;;;		<consequent>
;;;		<alternate>)

;;; Where <conditional> is any boolean expression that evaluates to #t or #f

;;; <consequent> is a scheme expression that will be evaluated,
;;; should <conditional> evaluate to #t

;;; And <alternate> is also a scheme expression that will be evaluated,
;;; should <conditional> evaluate to #f

;;; The value returned by the overall (if...) expression, is the value returned by the
;;; <consequent> expression or <alternate> expression depending on the respective truth
;;; value of the <conditional> expression

;;;; FOR EXAMPLE

(define num 20); => [ num : 20 ]

(if (> num 10)
	(* num 2)
	num); => 40

;;; Line by line interpretation
;;; We define num to be 20, now the symbol 'num is bound to the numeric value 20 in the
;;; environment

;;; If num is greater than 10, return the value of the expression (* num 2)
;;; Otherwise, return the value of num

;;; Since we have defined num to refer to the number 20, num is greater than 10, so we
;;; evaluate the first expression after the <conditional> (the <consequent>) and print 
;;; it's value to the REPL

;; Going back to the rpg character example, let's pretend that in our RPG game, we have
;; decided to implement our player character as a list of four values, HP, ATK, EXP, and LVL
;; These four values are numbers representing the health points, attack power, experience, and
;; current level of the character

(define character (list 10 20 95 2)); => [character : '(10 20 95 2)]
;; '(<HP> <ATK> <EXP> <LVL>)

;; Now let's say we want to define a procedure called level-up, which takes in a character
;; "object" (list of three numbers) and checks if the character's experience is >= 100.
;; If the character's experience points are sufficient, this procedure will double the 
;; health and attack of the character, increment the level by one, and reset the 
;; experience points to 0

;; As you might imagine, this procedure should and will use an (if...) expression
;; If the charcter's EXP is less than 100, simply return character, otherwise, return a new
;; list of values corresponding to the leveled up version of our character

;; YOU FILL IN
;;; I have provided helper procedures below
(define level-up (lambda (character)
	(if ...)))

;; Fill in the procedure until the following tests pass

(level-up character); => '(10 20 95 2)
(level-up (add-experience character 5)); => '(20 40 0 3)
(level-up (add-experience character 3)); => '(10 20 98 2)
(level-up (add-experience character 20)); => '(20 40 0 3)

;; HELPER PROCEDURES

;; Take in a character, and return the number corresponding to <HP>
(define get-health (lambda (character) (car character)))

(get-health character); => 10

;; Take in a character, and return the number corresponding to <ATK>
(define get-attack (lambda (character) (cadr character)))

(get-attack character); => 20

;; Take in a character, and return the number corresponding to <EXP>
(define get-experience (lambda (character) (caddr character)))

(get-experience character); => 95

;; Take in a character, and return the number corresponding to <LVL>
(define get-level (lambda (character) (cadddr character)))

(get-level character); => 2

;; Take in a character, and an amount of EXP points to add, and return
;; a new character with the original character's EXP plus amount
(define add-experience 
	(lambda (character amount)
		(list (get-health character)
			  (get-attack character)
			  (+ (get-experience character)
			     amount)
			  (get-level character))))

;; Example call

(add-experience character 10); => '(10 20 105 2)


