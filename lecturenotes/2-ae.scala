/*
 * Introduction to Scala and interpreters
 ****************************************
 
 This file can be executed with the scala interactive interpreter.
 Invoke 'scala' in the directory this file is in and then ':load 2-ae.scala'
 */

 
/* Case classes offer a convenient way to define 
 * a data type with variants, as they occur typically
 * in abstract syntax trees.
 *
 * Here is a simple example for a language that features
 * arithmetic expressions with variables 
 */

 // The sealed keyword means that all subclasses must be defined
// in this file. It enables completeness checks for pattern matches
sealed abstract class Exp 
case class Num(n: Int) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Mul(lhs: Exp, rhs: Exp) extends Exp
case class Id(x: Symbol) extends Exp 
// Symbols are similar to strings, but they are implicitly "canonicalized" and
// can hence be compared efficiently. Unlike strings, they can not be manipulated.
// They can be constructed by prefixing an identifier with a '

/* Here is a sample program written in this language, directly written
 * down using case class constructors */

 val test0 = Add(Mul(Id('x),Num(2)),Add(Id('y),Id('y)))

/* With a proper parser we could choose a syntax like "x*2+y+y".
 * We do not care much about concrete syntax and parsing, though.
 * That said, to make writing examples less verbose, Scala's implicits
 * come to the rescue.
 
 * Calls to implicit functions are inserted automatically by the compiler
 * if they help to restore well-typedness. For instance, we can define: */
 
implicit def num2exp(n: Int) = Num(n)
implicit def sym2exp(x: Symbol) = Id(x)

/* to lift integers and symbols to expressions. Using these implicits, the 
 * example can be written as: */ 

 val test = Add(Mul('x,2),Add('y,'y))

/* To give meaning to identifiers, we use _environments_. Environments are
 * mappings from Identifiers (which we represent as symbols) to Values.
 * In our simple language the only values are integers, hence: */
type Env = Map[Symbol,Int]

/* An evaluator (or interpreter) for this language takes an expression and
 * an environment as parameter and produces a value - in this case "Int".
 * 
 * The interpreter illustrates how pattern matching over case classes works.
 */
def eval(e: Exp, env: Env) : Int = e match {
  case Num(n) => n
  case Id(x) => env(x)
  case Add(l,r) => eval(l,env) + eval(r,env)
  case Mul(l,r) => eval(l,env) * eval(r,env)
}
/* A different (and arguably more 'object-oriented') way to implement this
 * evaluator would be to add an abstract "eval" method to the Exp class and
 * override it in all subclasses, each implementation corresponding to its
 * corresponding case in the pattern match. The choice between these alternatives
 * matters, since they support different dimensions of extensibility.
 *
 * We will mainly use the more functional style using pattern matching, because
 * it matches better to the order in which we present these topics in the lecture.
 *
 * To try the example, we need a sample environment that gives values to the 
 * (free) variables in the sample expression. The test environment also illustrates
 * how Scala supports direct definitions of constant maps.
 */

val testEnv = Map('x -> 3, 'y -> 4)

/* We can automatically test our evaluator using assert : */
assert( eval(test, testEnv) == 14)

/* We will now learn a different way to encode algorithms that operate on 
 * expressions (like the evaluator).
 * To this end, we will now use so-called "folds". Folds are well-known
 * for lists, but the concept is more general and applies to arbitrary
 * algebraic data types.
 *
 * We will present folds in such a style that they resemble visitors as known
 * from the OO design pattern literature. They correspond to so-called
 * "internal visitors" in which the traversal is encoded within the "accept" function.
 
 * An internal visitor consists of one function for each syntactic construct of the
   language. It has a type parameter that determines the "return type" of invoking the
   visitor on an expression. This type parameter is used in all positions in which
   the original syntax specifies a subexpression.
   
 * Internal visitors also correspond to a "bottom-up" traversal of the syntax tree. */ 

case class Visitor[T](num: Int => T, add: (T,T)=>T, mul: (T,T)=>T, id: Symbol=>T)
// an alternative to this design is to define num, add, mul, id as abstract methods
// and then create concrete visitors by subclassing or trait composition.


/* The fold function itself applies a visitor to an expression. 
 * Note that the recursion is performed in the fold function, hence
 * all visitors are not recursive.
 
 * Also note that this design enforces that all algorithms specified via this visitor
 * interfaces are compositional by design. This means that the recursion structure of
 * the algorithm corresponds to the recursion structure of the expression. Put in another
 * way, it means that the semantics (in terms of the meta-language) of a composite 
 * expression is determined by the semantics of the subexpressions; the syntax of the
 * subexpressions is irrelevant.

 * Compositional specifications are particularly nice because they enable "equational
 * reasoning": Subexpressions can be replaced by other subexpressions with the same 
 * semantics without changing the semantics of the whole.
 */
 
def foldExp[T](v: Visitor[T], e: Exp) : T = {
  e match {
    case Num(n) => v.num(n)
    case Id(x) => v.id(x)
    case Add(l,r) => v.add(foldExp(v,l),foldExp(v,r))
    case Mul(l,r) => v.mul(foldExp(v,l),foldExp(v,r))
  }
}
/* Here is our evaluator from above rephrased using the visitor infrastructure. */

val evalVisitor = Visitor[Env=>Int]( env=>_, (a,b)=>env=>a(env)+b(env), (a,b)=>env=>a(env)*b(env), x=>env => env(x)) 

/* We can of course also restore the original interface of eval */

def eval2(e: Exp, env: Env) = foldExp(evalVisitor,e)(env) 

/* Let's test whether it works. */

assert( eval2(test,testEnv) == 14)

/* We can of course also apply other algorithms using visitors, such as 
 * counting the number of "Num" literals, or printing to a string: */
val countVisitor = Visitor[Int]( _=>1, _+_, _+_, _=>0) 
val printVisitor = Visitor[String](_.n.toString, "("+_+"+"+_+")",_+"*"+_,_.x.toString)

def countNums(e: Exp) = foldExp(countVisitor,e) 

assert(countNums(test) == 1)

/* We will now learn a different "data-less" representation of expressions inspired by
 * the visitor infrastructure above. This representation is called "Church Encoding"
 * or sometimes, when used in statically typed languages, "Böhm-Berarducci Encoding".
 *
 * The idea of this representation is that the expression tree is a kind of intermediate
 * result that we can skip and go straight to the result of applying a visitor. That is,
 * we represent an expression by a function that will call the "right" functions of a visitor.
 * 
 * For instance, we want to represent the expression "test" by the function: */ 
 def foldForTest[T](v: Visitor[T]) : T = foldExp(v,test)  
/* except that we want to skip the construction (in the definition of test) and subsequent 
 * deconstruction(in the pattern match of foldExp) of the data type and represent the expression
 *  directly by the corresponding sequence of calls to the visitor.
 *
 * Recommended exercise: "Partially evaluate" foldForTest, that is, inline the definition of 
 * foldExp and specialize it to the case e = test.
 *
 * Hence, an expression is represented by a function that, for any type T, applies 
 * a Visitor[T] to itself and thereby produces a T.
 * 
 * The "for any type T" from the previous sentence means that ordinary Scala functions cannot
 * be used for this purpose, because all type parameters of Scala functions have to be known 
 * before a function can be passed as an argument. Rather, we encode it as an abstract class 
 * with an "apply" function that takes the type parameter.
 * 
 * Later in the course, or if you are a Haskell affectionado, you may recognize that this is
 * an encoding of the type "forall T.Visitor[T] => T", which means that functions accepting or 
 * returning ExpC values have so-called "rank-2 types".
 */

abstract class ExpC { def apply[T](v: Visitor[T]) : T } // we call this method "apply" because then 
                                                        // we can use function application syntax

implicit def num(n: Int) : ExpC = new ExpC {  // we use implicits for num and id again to make 
     def apply[T](v: Visitor[T]) = v.num(n) } // building expressions more concise.
implicit def id(x: Symbol) : ExpC = new ExpC {
     def apply[T](v: Visitor[T]) = v.id(x) }
def add(l: ExpC, r:ExpC) : ExpC = new ExpC {
     def apply[T](v: Visitor[T]) = v.add(l(v),r(v)) } // note the indirect recursion here!
def mul(l: ExpC, r:ExpC) : ExpC = new ExpC {
     def apply[T](v: Visitor[T]) = v.mul(l(v),r(v)) } // note the indirect recursion here!


/* We can again reconstruct the original eval interface. Note that the Visitor is 
  *_applied_ to the expression. */
def eval3(e: ExpC, env: Env) : Int = e(evalVisitor)(env)
     
// example test from above rewritten into Church encoding
val test2 : ExpC = add(mul('x,2),add('y,'y))
  
assert(eval3(test2, testEnv) == 14)

assert(test2(countVisitor) == 1)

/* As a voluntary but useful exercise, I recommend to start with a blank source file 
 * and reconstruct the definitions in this file from memory (or rather, derive them
 * from your understanding of the subject matter). 
 */
 
/* Further reading: 
 * - about the basics of interpreter design: PLAI Section 1+2
 * - about internal visitors in Scala see 
 *     Bruno C. d. S. Oliveira, Meng Wang, Jeremy Gibbons: 
 *     The visitor pattern as a reusable, generic, type-safe component. 
 *     OOPSLA 2008: 439-456
 *     online at http://www.cs.ox.ac.uk/jeremy.gibbons/publications/visitor.pdf
 */ 