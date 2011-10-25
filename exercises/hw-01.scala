/*
Homework for the course: 
Programming Languages and Types.


Hand in ...

  ... before Monday, October 31, 8:00 am in the morning.
  ... by email to pllecture @ informatik ...
  ... as attached Scala files task1.scala, task2.scala etc.

You can hand in in small groups. Mention the names of all group
members in the email and in all source files. Every group member
should understand the full solution. If possible, meet in person,
talk, and work together!
  
Copy the necessary definitions from the lecture notes.

Task 1: Variables in AE
=======================

Implement a Scala function that computes the set
of variables which occur in an AE program.

  a) with pattern matching

*/

def variables1(e : Exp) : Set[Symbol] = e match { ... }

/* 
  
  b) using a visitor and the foldExp function

*/

val variablesVisitor = Visitor[Set[Symbol]](..., ..., ..., ...)

def variables2(e : Exp) : Set[Symbol] = foldExp(variablesVisitor, e)

/* Some test cases, feel free to add more: */

assert(variables1('x) == Set('x))
assert(variables1(Add('x, 'y)) == Set('x, 'y))

assert(variables2('x) == Set('x))
assert(variables2(Add('x, 'y)) == Set('x, 'y))

/*
  Hint about sets in Scala:

  Use Set(1, 2, 3) to construct a set with fixed members.
  
  Use set1 + element to create a set with one element added.
  Use set1 - element to create a set with one element removed.
  Use set1 ++ set2 for the union of two sets.
  Use set1 -- set2 for the difference of two sets.

Task 2: Free variables in WAE
=============================

Implement a Scala function that computes the set
of all free (!) variables of a WAE term. 

*/

def freeVariables(wae : Exp) : Set[Symbol] = ...

/* Some test cases, feel free to define more: */

assert(freeVariables(Id('x)) == Set('x))
assert(freeVariables(With('x, 5, Add('x, 'y))) == Set('y))


/*

Task 3: Names in Scala
======================

Write three (different, interesting but short) Scala snippets
where a name is shadowed. For each snippet, explain which names
are binding, bound or free; and where the scopes of the binding
names are.

Task 4: De-Bruijn-Indices 
=========================

As we have seen, it is important to understand where each name is
bound. De-Bruijn-indices are an alternative representation of
terms where this binding structure is made explicit. Instead of
using symbols for bound names, it uses numeric indices. The index
#0 stands for the most recently introduced variable. The index #1
stands for the variable introduced before that. And so on. We
don't need symbols in the with expressions, either, because
everything is clear from the indices.

For example, the expression

  with (x = 11) {
    with (y = 2) {x * y} + 
    with (x = 5) {4 * x}
  }

is written as

  with (11) {
    with (2) {#1 * #0} +     // #0 refers to 2, #1 refers to 11
    with (5) {4 * #0}        // #0 refers to 5
  }
  
a) Convert (by hand) the following expression into a form with
   de-Bruijn-indices:
   
  with (a = 42) {
    with (b = a + 1) {
      with (c = a + b) {
        a + b + c
      }
    }
  }

b) De-Bruijn-indices can be used to implement a check for
   alpha-equivalence, that is, to test whether two expressions
   are identical except for the names of variables. Explain how
   that would work.

c) Expressions in de-Bruijn form can be represented in Scala with
   the following case classes:

*/

sealed abstract class ExpDB 
case class NumDB(n: Int) extends ExpDB
case class AddDB(lhs: ExpDB, rhs: ExpDB) extends ExpDB
case class MulDB(lhs: ExpDB, rhs: ExpDB) extends ExpDB
case class IdDB(index : Int) extends ExpDB 
case class WithDB(xdef: ExpDB, body: ExpDB) extends ExpDB

/* 
   Implement a Scala function that converts from WAE expressions
   to ExpDB. 
   
   Hint 1: The local helper function takes a list of variable
   names currently in scope as additional parameter. You can use
   this list to figure out the index to use for a bound variable.
   
   Hint 2: Throw an error if you encounter an unbound variable.
*/

def convert(wae : Exp) : ExpDB = {
  def helper(wae : Exp, names : List[Symbol]) : ExpDB = 
    wae match {
      ...
    }
  
  helper(wae, List())
}

/* A test case, feel free to add more: */

assert(
  convert(With('x, 11, Add(With('y, 2, Mul('x, 'y)), With('x, 5, Mul(4, 'x))))) ==
  WithDB(11, AddDB(WithDB(2, MulDB(IdDB(1), IdDB(0))), WithDB(5, MulDB(4, IdDB(0))))))


