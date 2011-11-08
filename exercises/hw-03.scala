/*
Homework for the course: 
Programming Languages and Types.


Hand in ...

  ... before Monday, November 14, 8:00 am in the morning.
  ... by email to pllecture @ informatik ...
  ... as attached files

You can hand in in small groups. Mention the names of all group
members in the email and in all source files. Every group member
should understand the full solution. If possible, meet in person,
talk, and work together!
  
Copy the necessary definitions from the lecture notes.


Task 1: Language primitives
===================

It is unsatisfactory to have many language primitives (such as `if0`).
It would be more elegant if there would just be a number of "pre-installed"
functions that are called using normal function application.

Let's say you define a function `myif0` as a F1WAE-function of three parameters
(ignore the problem that our functions can only have one parameter). The parameters
are `if-part`, `then-part` and `else-part`, and `(myif0 if-part then-part else-part)`
is just implemented as `(if0 if-part then-part else-part)`.

Think about whether you could use `myif0` rather than the builtin `if0` in your
factorial function. Explain why or why not. If not, think about a possible fix
that would let you use `myif0`.

Hint: think of call-by-value versus call-by-name.



Task 2: Infinite lists
==============

Write a Haskell program that constructs a list of:

(i) all natural numbers
(ii) all even natural numbers
(iii) all squared integers
(iv) all factorials, i.e. the infinite sequence `1,1,2,6,24,120,720,...`

Discuss why infinite lists are useful.


Task 3: Call-by-name parameters in Scala
================================

Look up call-by-name parameters in Section 4.6.1 of the Scala language
specification.

(i) Write a simple Scala program that uses call-by-name parameters.
(ii) Consider the following definitions:
    
    def whileLoop1 (cond:    Boolean) (body:    Unit): Unit = while(cond) body
    def whileLoop2 (cond: => Boolean) (body: => Unit): Unit = while(cond) body

  Explain why the following program works or does not work with `whileLoop1` and
  `whileLoop2` respectively.

    var i = 10
    whileLoopXXX(i > 0) {
      print(i)
      i = i - 1
    }


Task 4: Lazy evaluation
===============

Lazy evaluation can modularize backtracking algorithms in an elegant way. Here
is a solution to the 8-queens problem in Haskell using lazy evaluation:

    boardSize = 8
    
    queens 0 = [[]]
    queens n = [ x : y | y <- queens (n-1),
                         x <- [1..boardSize], safe x y 1]
    
    safe x [] n = True
    safe x (c:y) n = x /= c &&
                     x /= c + n &&
                     x /= c - n &&
                     safe x y (n+1)
    
    main = print (queens 8)

Understand how this program works. What happened to the usual backtracking?
(you don't need to write this in your hand-in). If you don't understand the
syntax in the 4th line, see
http://www.haskell.org/haskellwiki/List_comprehension.


Compare the given program with a typical "eager" solution such as
http://www.cs.princeton.edu/introcs/23recursion/Queens.java.html. What would you
have to do in both version, respectively, to


(i) compute only the first 5 solutions and then stop,
(ii) compute the number of solutions,
(iii) find all solutions where the queen in column 3 is at row 5?

You do not need to describe the actual changes or implementations of (i), (ii)
and (iii). Instead outline briefly what you would have to do in both versions,
and describe to what extend the two versions differ.





*/
