# scheme-workshop
Source code for scheme examples that will be covered during the Fall 2017 Scheme Workshop

- Please install GNU Guile.
  - `msys2` has a way of already doing this:

    pacman -S guile

  - `brew` with Mac OS X:

    brew install guile

  - `apt-get` in Ubuntu/Debian:

    sudo apt-get install guile-2.0

- Then, run all of the following files by doing this:

    guile NAME.scm

- To run Guile in interactive mode, run:

    guile

# Topics

- Basic Scheme
  - Simple Expressions
    - Constants
    - Arithmetic expressions
    - Nested Arithmetic
  - Variables
  - Definitions
  - Procedure definitions/lambda expressions
  - Procedure calls
  - Recursion
    - Write Factorial recursive
  - Lists/pairs and Cons and Cdr
  - Recursion with lists and mapping
  - Tail Recursion
    - Write Factorial tail recursive
  - Closures
  - Mutation, state, and abstract objects
  - Continuations
- Implementing Scheme
  - The Heap Based Model
  - Environment
  - Frames and Control Stack
  - Closures
  - Continuations
