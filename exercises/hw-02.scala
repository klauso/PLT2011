/*
Homework for the course: 
Programming Languages and Types.


Hand in ...

  ... before Monday, November 7, 8:00 am in the morning.
  ... by email to pllecture @ informatik ...
  ... as attached Scala files task1.scala, task2.scala etc.

You can hand in in small groups. Mention the names of all group
members in the email and in all source files. Every group member
should understand the full solution. If possible, meet in person,
talk, and work together!
  
Copy the necessary definitions from the lecture notes.

* 
Task 1: First-class functions
=======================

Implement examples that make use of first-class functions and could not be
written nicely with first-order functions only.
(i) Implement three examples in Scala. 
(ii) Write 3 test cases for each example.
(iii) Implement three examples in FAE. You may extend FAE for that purpose.
(iv) Write 3 test cases for each FAE example.
(v) Discuss the difference between first-order functions and first-class
    functions. What are the respective advantages/disadvantages?
*/

/*
Task 2: Dynamic scoping
=======================

Recap: Why is dynamic scoping bad? Give examples in FAE.


Task 3: Names in FAE
======================

Write three (different, interesting but short) FAE snippets
(note: FAE) where a name is shadowed. For each snippet, explain
which names are binding, bound or free; and where the scopes of
the binding names are.



Task 4: If Zero
===============

Solve Exercise 4.3.1 in the textbook, that is, add an `If0`
construct to all relevant definitions of FAE. Furthermore, add
more testcases.

*/

assert( eval(If0(0, 1, 2))  == 1 )
assert( eval(If0(17, 1, 2)) == 2 )
