import scala.collection.mutable.ArraySeq

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

case class NewBox(e: Exp) extends Exp // create a new box
case class SetBox(b: Exp, e: Exp) extends Exp // assign to a box
case class OpenBox(b: Exp) extends Exp // read value in a box
case class Seq(e1: Exp, e2: Exp) extends Exp // sequencing of expressions


val test = wth('b, NewBox(0), 
             Seq(
               SetBox('b, Add(1, OpenBox('b))), 
               OpenBox('b)))

 
val test2 = wth('a, NewBox(1),
              wth('f, Fun('x, Add('x, OpenBox('a))),
                Seq(SetBox('a,2),
                    App('f,5))))                
 

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
               
case class AddressV(a: Int) extends Value

class Store(size: Int) extends ArraySeq[Value](size) {
  var nextFreeAddr : Int = 0
  def nextAddress : Int = {
    val x = nextFreeAddr
    nextFreeAddr += 1
    x
  }
}

/* We will often need a fresh address in the store. We do so using a counter variable. */


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

def eval(e: Exp, stack: List[Env], s: Store) : Value = e match {
  /* All expressions whose evaluation does not alter the store just return s. */
  case Num(n: Int) => NumV(n)
  case Id(x) => stack.head(x)
  case f@Fun(param,body) => ClosureV(f, stack.head)
  /* In recursive cases we have to thread the store through the evaluation. In particular, we
   * define the order of evaluation explicitly through data flow dependencies. */
  case If0(cond, thenExp, elseExp) => eval(cond,stack,s) match {
    case NumV(0) => eval(thenExp,stack,s)
    case _ => eval(elseExp,stack,s)
  }    
  case Add(l,r) => {
    (eval(l,stack,s), eval(r,stack,s)) match {
      case (NumV(v1),NumV(v2)) => NumV(v1+v2)
      case _ => sys.error("can only add numbers")
    }
  }
  case App(f,a) => eval(f,stack,s) match {
    case ClosureV(f,closureEnv) =>  eval(f.body, (closureEnv + (f.param -> eval(a,stack,s))) :: stack, s)
    case _ => sys.error("can only apply functions")
  }
  /* In a sequence, we ignore the result of evaluating e1 but not its effect on the store. */
  case Seq(e1,e2) => eval(e1,stack,s); eval(e2,stack,s) 

  /* A new box is created by putting it into the store at a new address .*/
  case NewBox(e: Exp) =>  {
       val a = s.nextAddress
       s.update(a, eval(e,stack,s))
       AddressV(a)
  }
  /* Setting a box is now a two-step process: First evaluate b to an address, then lookup and update
  * the value associated to the address in the store. Note that "updated" is a functional method. */
  case SetBox(b: Exp, e: Exp) => eval(b,stack,s) match {
    case AddressV(a) => {
        val ev = eval(e,stack,s)
        s.update(a,ev)
        ev
    }
    case _ => sys.error("can only set boxes")
  }
  /* OpenBox uses the same two-step process but does not update the store. */
  case OpenBox(b: Exp) => eval(b,stack,s) match {
    case AddressV(a) => s(a)
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

 def gc(roots: Set[Int], s: Store) : Unit = {
  def allAddrInVal(v: Value) : Set[Int] = v match {
    case AddressV(a) => Set(a)
    case NumV(_) => Set.empty
    case ClosureV(f,env) => allAddrInEnv(env)
  }
  def allAddrInEnv(env: Env) : Set[Int] = env.values.map(allAddrInVal _).fold(Set.empty)(_ union _)
  def mark(seed: Set[Int]) : Set[Int] = {
    val newaddresses = seed.flatMap( ad => allAddrInVal(s(ad))) 
    if (newaddresses.subsetOf(seed)) seed else mark(seed union newaddresses)
  }
  val marked = mark(roots) // mark ...
  s.indices.foreach( idx => if (! marked(idx)) s.update(idx, null)) // and sweep!
}

val teststore = new Store(20)
teststore.update(10, ClosureV(Fun('x,'y), Map('y -> AddressV(8))))
teststore.update(6, NumV(42))
teststore.update(9, AddressV(7))
teststore.update(7, NumV(6))
teststore.update(8, AddressV(6))
gc(Set(10), teststore)


/* Note that garbage collectors only _approximate_ the set of semantically disposable 
 * store entities. Even with garbage collectors, applications may very well suffer from
 * memory leaks. The approximation should be _safe_, in the sense that a datum is never
 * reclaimed when it is used by subsequent computations. Furthermore, it must reclaim
 * enough garbage to be actually useful - reachability has turned out to be a rather
 * useful (and sound) approximation of semantic disposability.  Garbage collectors must
 * also be efficient. Efficiency of GC is a huge research topic that we are not going to 
 * discuss. One efficiency problem with garbage collectors based on reachability that we want to
 * mention is the "stop-the-world" phenomenon.
 *
 * Another point to note is that the above gc algorithm would not work with circular environments
 * as we constructed them for LetRec.
 */
 
 