#lang racket
;; As mentioned in the previous module concerning closures, closures allow us to model and create
;; abstract objects with internal state

;; In order to harness the full power of closures to be able to implement abstract objects, we need
;; to introduce a new concept known as "state" and "mutability"

;; So far, every scheme expression you have seen will always produce the same output value given
;; the same input value

;; So far, we have assumed that once we do something like

(define a 3)

;; That the symbol 'a can, and will only ever refer to the value 3 in the current environment
;; This adheres to the familiar notion of a variable from mathematics

;; For example, in algebra, when we say 5 + x = 10, we know that this means that x refers to the
;; value 5
;; From an algebraic point of view, it would make no sense to try and say something like
;; "x is an object that now refers to the number 5, but if I want to, I can later make it refer to
;; a different number like 10."
;; This would make absolutely no sense algebraically, as we already said 5 + x = 10, and 5 + 10 != 10

;; However, in real life, it is often useful to view the world as being populated by independent
;; objects, each of which has a local state that can change (or be modified) over time

;; An object is said to "have state" or be "stateful" if its behavior is influenced by its history

;; For example, a refridgerator has state in that the result of the query "I can has food?!?" depends
;; upon the history of food deposits and food withdrawals that have taken place between you
;; (and potentially other users) and the refridgerator in question

;; If we were operating under a purely expression oriented model, we might expect that any time we
;; go to take food out of the fridge, we will always get the same amount (and likely same type) of food
;; that we did the n-many other times that we took food out of the fridge

;; While this might sound elegant from a mathematical perspective, it is simply not how the real world
;; works

;; When an object has state, we often say that such an object is "mutable", meaning that the state
;; of the object can change over time (a.k.a the object has "mutability")

;; The way that scheme handles this notion of state and mutability is via a built-in primitive
;; procedure called 'set!'

;; What the set! procedure does is take in a symbol that is currently bound to a value in the
;; environment, and a new value to bind that symbol too, and basically replaces the old value
;; with the new value in the respective binding record

;; EXAMPLE

(define number 5)
number ;=> 5
;; ENVIRONMENT: [number : 5]

(set! number 10)
number ;=> 10
;; ENVIRONMENT: [number : 10]

;; By combining judicious use of the set! procedure, with closures that maintain an environment with
;; bindings, we can easily implement abstract objects
;; In a sense, the combination of first-class closures and the set! procedure constitutes a simple
;; and elegant object system, similar to the Object-based style of OOP seen in Javascript

;; For example, let us define a procedure called make-fridge, that creates a "closure object"
;; representing an abstract refridgerator object

;; NOTE: Nested defines are syntactically valid in scheme, each define will be evaluated in succession
;; with each of the bindings held in the environment of the closure object returned by the make-fridge
;; procedure

;; NOTE: The (begin ...) syntactic form allows for us to specify expressions that are to be evaluated
;; one after the other
;; begin takes in an arbitrary number of expressions, and evaluates each one in succession, returning
;; the value of the last expresssion as the value of the overall (begin exp1 exp2 ...) expression

(define make-fridge
  (lambda (starting-food)
    (define add-food
      (lambda (amount)
        (begin
          (set! starting-food (+ starting-food amount))
          starting-food)))
    (define remove-food
      (lambda (amount)
        (if (< (- starting-food amount) 0)
            'NOT_ENOUGH_FOOD
            (begin
              (set! starting-food (- starting-food amount))
              starting-food))))
    (define delegate
      (lambda (message)
        (if (eq? message 'add)
            add-food
            (if (eq? message 'remove)
                remove-food
                'ERROR))))
    delegate))

(define fridge (make-fridge 100))
; => A closure object (the delegate procedure) that maintains an internal environment with the
;; following set of bindings
;; [ add-food : (lambda (amount) ...), remove-food : (lambda (amount) ...), starting-food : 100]

((fridge 'add) 20); => 120
;; Now the environment maintained in the fridge closure object will look like the following
;; [ add-food : (lambda (amount) ...), remove-food : (lambda (amount) ...), starting-food : 120]

((fridge 'remove) 30); => 90
;; Now the environment maintained in the fridge closure object will look like the following
;; [ add-food : (lambda (amount) ...), remove-food : (lambda (amount) ...), starting-food : 90]

;; For clarity, we will walk through the evaluations that take place when we pass various messages
;; to the fridge "closure object" on the board


