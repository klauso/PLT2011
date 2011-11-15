/*
These are lecture notes for the "Programming Languages and Types" course by Klaus Ostermann 
at the University of Marburg

loosely based on Sec. 6 of "Programming Languages: Application and Interpretation" by
Shriram Krishnamurthi

Please comment/correct/improve these notes via github. Proposals or questions can
be submitted as an "issue"; proposals for corrections/extensions/improvements can
be submitted as a "pull request". You can of course also send an email to Klaus Ostermann */

/* Today we study _mutation_. More specifically, we want to equip our language with mutable data
 * structures. Typical mutable data structures in common languages include objects with mutable
 * fields or structures/records in languages like C or Pascal. 
 *
 * We will study a particularly simple mutable data structure: Boxes. In OO parlance, boxes
 * can be thought of as an object with a single field that can be mutated. Despite their simplicity,
 * boxes already illustrate all main issues associated with adding mutable state to a language.
 *
 * A different and less interesting form of mutation is the mutability of _variables_, such as the
 * possibility to assign something to a 'local' variable bound via a lambda or "with". We will not
 * talk about mutable variables today. 
 *
 * We will add boxes to our base language, FAE: */

 sealed abstract class Exp
case class Num(n: Int) extends Exp
case class Id(name: Symbol) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class If0(cond: Exp, thenExp: Exp, elseExp: Exp) extends Exp
implicit def num2exp(n: Int) = Num(n)
implicit def id2exp(s: Symbol) = Id(s)
case class Fun(param: Symbol, body: Exp) extends Exp
case class App (funExpr: Exp, argExpr: Exp) extends Exp
def wth(x: Symbol, xdef: Exp, body: Exp) : Exp = App(Fun(x,body),xdef)

/* To add mutation to FAE, we add four language constructs: */

case class NewBox(e: Exp) extends Exp // create a new box
case class SetBox(b: Exp, e: Exp) extends Exp // assign to a box
case class OpenBox(b: Exp) extends Exp // read value in a box
case class Seq(e1: Exp, e2: Exp) extends Exp // sequencing of expressions

/* Let's consider a sample program in this language. */

val test = wth('b, NewBox(0), 
             Seq(
               SetBox('b, Add(1, OpenBox('b))), 
               OpenBox('b)))

/* Let's consider the question how the interpreter branch for sequencing could look like. 
 * Here is an attempt:
 
   case Seq(e1,e2) => {
      eval(e1,env)
      eval(e2,env)
   }
   
 * However, this cannot possibly be correct. As long as our interpreter does not use mutation,
 * there is no way how the evaluation of e1 could have any effect on the evaluation of e2.
 * However, this must obviously be the case, as the example above illustrates.
 *
 * To shed light on the actual nature of mutation, we will not use mutation in our interpreter.
 * (cf. discussion about meta-interpretation vs syntactic interpretation in one of the following
 *  lectures)
 * One possibility is that the interpreter always returns a possibly updated environment in addition
 * to the computed value. However, this does not work: First, it would introduce once more a form
 * of dynamic scoping (which we want to avoid), and second, it would not work when closures 
 * are involved. Consider the following example: */
 
val test2 = wth('a, NewBox(1),
              wth('f, Fun('x, Add('x, OpenBox('a))),
                Seq(SetBox('a,2),
                    App('f,5))))                
 
/* The mutation should affect the box stored in the closure bound to f. But with the implementation
 * strategy described above it would not. 
 *
 * Note that changing the value of a in the example is not a violation of static scope. Scoping tells
 * us which occurence of an identifier is bound to which introduction of an identifier, but not its value.
 *
 * Hence, the following two evaluation strategies are both flawed:
 * 1) Using the environment (which maps a to 1) stored in the closure for f when evaluating f(5).
 *    The program will evaluate to 6 rather than 7.
 * 2) Using the environment present at the time of procedure invocation: f(5). This will record the
 *    change to a but reintroduces dynamic scope.
 *
 * Insight: We need _two_ repositories of information. The environment is the guardian of static scope and it
 * maps symbols to values. */

 sealed abstract class Value
type Env = Map[Symbol, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value
 
/* 
 * The other, which we call _store_, is responsible for tracking dynamic changes.
 *
 * Determining the value inside a box will become a two-step process: We first evaluate the box expression
 * to an _address_, and then use the store to lookup the value stored at that address. We choose to represent
 * addresses by integers.
 */ 
               
type Address = Int
case class AddressV(a: Int) extends Value

type Store = Map[Address, Value]

/* We will often need a fresh address in the store. We do so using a counter variable. */
var _nextAddress : Address = 0
def nextAddress : Address = {
  _nextAddress += 1
  _nextAddress
}
/* Note: We promised to implement the interpreter without using mutation. Here we did use mutation, but
 * this usage of mutation is not essential: Instead we could just search for the largest address in the
 * present store and add one to it.
 *
 * Let's now discuss the evaluate for FAE with conditionals and boxes, BCFAE. To this end, consider
 * the following example program: */

val test3 = wth('switch, NewBox(0),
             wth('toggle, Fun('dummy, If0(OpenBox('switch),
                                          Seq(SetBox('switch, 1), 1),
                                          Seq(SetBox('switch, 0), 0))),
               Add(App('toggle,42), App('toggle,42))))                                          

/* This program shoud return 1.
 * Let's discuss on the blackboard what the environment and store 
 * should look like during the evaluation of this program.
 *
 * Insight: We must pass the current store in and updated store out of every expression's evaluation.
 * This is called store-passing style. Consequently, we have to update the type of our evaluator: */

def eval(e: Exp, env: Env, s: Store) : (Value, Store) = e match {
  /* All expressions whose evaluation does not alter the store just return s. */
  case Num(n: Int) => (NumV(n),s)
  case Id(x) => (env(x),s)
  case f@Fun(param,body) => (ClosureV(f, env),s)  
  /* In recursive cases we have to thread the store through the evaluation. In particular, we
   * define the order of evaluation explicitly through data flow dependencies. */
  case If0(cond, thenExp, elseExp) => eval(cond,env,s) match {
    case (NumV(0),s2) => eval(thenExp,env,s2)
    case (_,s2) => eval(elseExp,env,s2)
  }    
  case Add(l,r) => {
    eval(l,env,s) match {
      case (NumV(v1),s2) => eval(r,env,s2) match {
        case (NumV(v2),s3) => (NumV(v1+v2),s3)
        case _ => sys.error("can only add numbers")
      }
      case _ => sys.error("can only add numbers")
    }
  }
  case App(f,a) => eval(f,env,s) match {
    case (ClosureV(f,closureEnv),s2) => eval(a,env,s2) match {
      case (av,s3) => eval(f.body, closureEnv + (f.param -> av), s3)
    }
    case _ => sys.error("can only apply functions")
  }
  /* In a sequence, we ignore the result of evaluating e1 but not its effect on the store. */
  case Seq(e1,e2) => eval(e2,env,eval(e1,env,s)._2) 

  /* A new box is created by putting it into the store at a new address .*/
  case NewBox(e: Exp) => eval(e,env,s) match {
    case (v,s2) => {
       val a = nextAddress
       (AddressV(a), s2 + (a -> v))
    }
  }
  /* Setting a box is now a two-step process: First evaluate b to an address, then lookup and update
  * the value associated to the address in the store. Note that "updated" is a functional method. */
  case SetBox(b: Exp, e: Exp) => eval(b,env,s) match {
    case (AddressV(a),s2) => eval(e,env,s2) match {
       case (ev, s3) => (ev, s3.updated(a,ev))
    }
    case _ => sys.error("can only set boxes")
  }
  /* OpenBox uses the same two-step process but does not update the store. */
  case OpenBox(b: Exp) => eval(b,env,s) match {
    case (AddressV(a),s2) => (s2(a),s2)
    case _ => sys.error("can only open boxes")
  }
}

/* From an implementation point of view, our interpreter has the problem that nothing is ever removed
 * from the store. One possibility would be to add an operation "removeBox" or the like to the language,
 * but this would lead to dangling pointers and all the problems associated with manual memory management.
 *
 * Our model of stores is sufficient to illustrate how modern languages deal with memory management:
 * By garbage collection. Garbage collectores automatically reclaim memory that is no longer referenced
 * from within the active part of the computation. We can model a (naive) mark-and-sweep garbage collector
 * as follows: */

 def gc(env: Env, s: Store) : Store = {
  def allAddrInVal(v: Value) : Set[Address] = v match {
    case AddressV(a) => Set(a)
    case NumV(_) => Set.empty
    case ClosureV(f,env) => allAddrInEnv(env)
  }
  def allAddrInEnv(env: Env) = env.values.map(allAddrInVal _).fold(Set.empty)( _ union _)
  def mark(seed: Set[Address]) : Set[Address] = {
    val newaddresses = seed.map( ad => allAddrInVal(s(ad))).fold(Set.empty)(_ union _)
    if (newaddresses.subsetOf(seed)) seed else mark(seed union newaddresses)
  }
  val marked = mark(allAddrInEnv(env)) // mark ...
  s.filterKeys(marked(_)) // and sweep!
}

val teststore = Map(10 -> ClosureV(Fun('x,'y), Map('y -> AddressV(8))), 6 -> NumV(42), 9 -> AddressV(7), 7 -> NumV(6), 8 -> AddressV(6))
assert(gc(Map('a -> AddressV(10)), teststore) == teststore - 7 - 9)           