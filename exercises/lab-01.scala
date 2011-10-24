/*
Programming Languages and Types

Lab Session, October 24, 2011


Topics for today:

  * GitHub
  * Scala introduction
  * Visitors and Church encoding

*/

// GIT HUB
// -------

/* If you want to change something in the lecture material:

   1. clone https://github.com/klauso/PLT2011 to your own github account.
   2. change something.
   3. commit the change (to your clone of the PLT2011 repo)
   4. Issue a pull request for the change.
   5. Lecture stuff will see your proposed change and decide
      whether and how to merge it into the main PLT2011 repo.
   
   Alternatively: Instead of editing the file on github, clone
   your github clone of the PLT2011 repo to your local machine,
   change and commit there, and push back to your github clone.
   
   If you want to ask a question or propose some change in the
   lecture:
   
     * Open an issue at https://github.com/klauso/PLT2011/issues.
   
   Or alternatively:
   
     * Send an email.
*/

// FUNCTIONS
// ---------

/* The syntax of method definitions. */

def twice(x : Int) : Int = 2 * x

/* Functions are first-class values and can be taken as arguments. */
def apply_to_twenty_one(f : Int => Int) : Int = f(21)

/* Functions are just objects with an apply function. */
val test1 = apply_to_twenty_one(new Function1[Int, Int] {
  def apply(arg : Int) : Int = 2 * arg
})

/* Syntactic sugar for the above. */
val test2 = apply_to_twenty_one((arg : Int) => 2 * arg)

/* Sometimes, the argument type can be inferred. */
val test3 = apply_to_twenty_one(arg => 2 * arg)

/* Underscore notation for functions. */
val test4 = apply_to_twenty_one(2 * _)

/* Method names can be used as functions. */
val test5 = apply_to_twenty_one(twice)

// Lists
// -----

/* A list value. Values can not be changed after their definition. */
val list123 = List(1, 2, 3)

/* Lists are stored as linked lists, using a class :: and a class Nil. */
val list123 = 1 :: 2 :: 3 :: Nil

/* Note that :: is right-associative, so the above means: */
val list123 = 1 :: (2 :: (3 :: Nil))

/* Deconstructing a list by pattern matching. */

val test = list123 match {
  case Nil => "empty"
  case x :: xs => "starts with " + x
}

/* Exercise: The sum of a list of integers. */

def sum(list : List[Int]) : Int = 
  // ...

/* Exercise: The product of a list of integers. */

def product(list : List[Int]) : Int = 
  // ...

/* Exercise: Code reuse by abstracting over the differences
   between sum and product. */
   
def fold[T](list : List[Int], neutral : T, f : (Int, T) => T) : T =
  // ...

def sum2(list : List[Int]) = fold[Int](list, 0, _ + _)
def product2(list : List[Int]) = fold[Int](list, 1, _ * _)

// VISITORS
// --------

/* In a visitor, we collect the operations (like neutral and f
   above). We call the operations after the case classes. Note
   that for historical reasons, "::" is pronounced "cons" for 
   "constructor". */
   
case class ListVisitor[T](nil : T, cons : (Int, T) => T)

/* Visitors can be used for folding data. */
def foldVisitor[T](list : List[Int], visitor : ListVisitor[T]) : T =
  list match {
    case Nil => visitor.nil
    case head :: tail => visitor.cons(head, foldVisitor(tail, visitor))
  }

val sumVisitor = ListVisitor[Int](0, _ + _)
def sum3(list : List[Int]) = foldVisitor[Int](list, sumVisitor)

val productVisitor = ListVisitor[Int](1, _ * _)
def product3(list : List[Int]) = foldVisitor[Int](list, productVisitor)

// CHURCH ENCODING
// ---------------

/* But visitors can also be used for representing data. That is,
   we can fold over a list without actually having that list.

   To see how that works, we start with the function which
   reduces the list List(1, 2, 3, 4) according to some visitor. */

def visit1234a[T](visitor : ListVisitor[T]) : T =
  foldVisitor(1 :: 2 :: 3 :: 4 :: Nil, visitor)

/* We inline foldVisitor: */

def visit1234b[T](visitor : ListVisitor[T]) : T =
  1 :: 2 :: 3 :: 4 :: Nil match {
    case Nil => visitor.nil
    case head :: tail => visitor.cons(head, foldVisitor(tail, visitor))
  }

/* We simplify the pattern match: */

def visit1234c[T](visitor : ListVisitor[T]) : T =
  visitor.cons(1, foldVisitor(2 :: 3 :: 4 :: Nil, visitor))

/* Inline and simplify again: */

def visit1234d[T](visitor : ListVisitor[T]) : T =
  visitor.cons(1, visitor.cons(2, foldVisitor(3 :: 4 :: Nil, visitor)))

/* And again: */

def visit1234d[T](visitor : ListVisitor[T]) : T =
  visitor.cons(1, visitor.cons(2, visitor.cons(3, foldVisitor(4 :: Nil, visitor))))

/* And again: */

def visit1234d[T](visitor : ListVisitor[T]) : T =
  visitor.cons(1, visitor.cons(2, visitor.cons(3, visitor.cons(4, visitor.nil))))
  
/* The list is gone, but we can still fold over it. */

val sum1234 = visit1234d(sumVisitor)
val product1234 = visit1234d(productVisitor)

/* But what about other lists? */
