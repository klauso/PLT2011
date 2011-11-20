/* Let us now consider a more accurate modeling of garbage collection (gc).
 * This time, we will use a mutable store instead of a functional store,
 * because our purpose is not to explain mutation but to explain gc.
 */

import scala.collection.mutable.ArraySeq

/* This is the well-known syntax of our language: FAE with boxes */
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

/* We will equip our values with a mutable flag that is useful for mark-and-sweep
 * garbage collection. In real systems it is implemented as a bit flag, or, if
 * the so-called "tri-color algorithm" is used, with two bit flags. */

abstract class Value {
  var marked : Boolean = false
}

/* We will also use a mutable map instead of a map for environments.
 * This not needed for mark-and-sweep, but for copying garbage collectors
 * such as Cheney's semi-space garbage collection algorithm. */
type Env = scala.collection.mutable.Map[Symbol, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value
case class AddressV(a: Int) extends Value

/* To be able to experiment with different store and gc designs, we create an interface
 * for stores. The stack parameter in malloc is needed during gc to determine
 * the root nodes from which the algorithms can start. */
 
trait Store {
  def malloc(stack: List[Env], v: Value) : Int
  def update(idx: Int, v: Value) : Unit
  def apply(idx: Int) : Value
}

/* In our interpreter, the stack
 * of environments is only implicitly available on the stack of the meta-language.
 * To reify the call-stack we need to make it explicit. We do so by constructing the
 * stack explicitly and passing it as parameter. The first element of the stack
 * is the current environment; the rest is only needed for gc. */

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
  case Seq(e1,e2) => eval(e1,stack,s); eval(e2,stack,s) 

  case NewBox(e: Exp) =>  {
       val a = s.malloc(stack, eval(e,stack,s))
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

/* Here is one implementation of the Store interface that does not perform gc.
 * It just runs of memory once the store is full. */
 
class NoGCStore(size: Int) extends Store {
  val memory = new ArraySeq[Value](size)
  var nextFreeAddr : Int = 0
  def malloc(stack: List[Env], v: Value) : Int = {
    val x = nextFreeAddr
    if (x >= size) sys.error("out of memory")
    nextFreeAddr += 1
    update(x,v)
    x
  }
  def update(idx: Int, v: Value) : Unit = memory.update(idx,v)
  def apply(idx: Int) = memory(idx)
}

/* Here is a model of a mark-and-sweep garbage collection. */

class MarkAndSweepStore(size: Int) extends Store {
  val memory = new ArraySeq[Value](size)
  var free : Int = size
  var nextFreeAddr : Int = 0

  def malloc(stack: List[Env], v: Value) : Int = {
    if (free == 0) gc(stack)
    if (free == 0) sys.error("out of memory")
    while (memory(nextFreeAddr) != null) {
      nextFreeAddr += 1
      if (nextFreeAddr == size) nextFreeAddr = 0
    }
    free -= 1
    update(nextFreeAddr,v)
    nextFreeAddr
  }

  def update(idx: Int, v: Value) : Unit = memory.update(idx,v)
  def apply(idx: Int) = memory(idx)

  def allAddrInVal(v: Value) : Set[Int] = v match {
    case AddressV(a) => Set(a)
    case NumV(_) => Set.empty
    case ClosureV(f,env) => allAddrInEnv(env)
  }
  def allAddrInEnv(env: Env) : Set[Int] = env.values.map(allAddrInVal _).fold(Set.empty)(_ union _)

  def mark(seed: Set[Int]) : Unit = {
    val newaddresses = seed.flatMap( ad => allAddrInVal(memory(ad))).filter( ! memory(_).marked) 
    if (newaddresses != Set.empty) {
      newaddresses.foreach( memory(_).marked = true)
      mark(newaddresses)
    }
  }
  def sweep() : Unit = {
    memory.indices.foreach( 
      idx => if ((memory(idx) != null) && memory(idx).marked) {
       memory(idx).marked = false
       } else {
       free += 1
       memory(idx) = null
    })
  }                                    

  def gc(stack: List[Env]) : Unit = {
      println("starting gc, stack = "+stack+", store = "+memory)
      mark(stack.map(allAddrInEnv _).fold(Set.empty)(_ union _))
      sweep()
      println("gc complete, store = "+memory+", number of free slots = "+free)      
  }
}

val test4 = wth('makedata, Fun('x, NewBox(NewBox(NewBox('x)))),
                wth('s, App('makedata,1),
                  Seq(App('makedata,2),
                    Seq(App('makedata,3),
                        App('makedata,'s)))))
                        
eval(test4, List(scala.collection.mutable.Map.empty), new MarkAndSweepStore(4))    

/* This model of garbage collection does not illustrate the difficulty of 
 * memory management. In most languages, the size of the allocated memory regions
 * on the heap vary, and hence one needs an algorithm to find a free and large-enough spot
 * on the heap. There are various algorithms and heuristics (best-fit, worst-fit, first-fit, ...)
 * for that purpose.
 *
 * There are also various alternative gc designs. Mark and sweep is a non-moving algorithm,
 * where reachable heap objects are never moved. In contrast to that, copying gc algorithms
 * move the reachable object to a different portion of the heap. One of the oldest algorithms
 * is the semi-space garbage collector, in particular with the implementation proposed by Cheney (google it!).
 *
 * Topic for class discussion: What are the pros and cons of moving vs non-moving gc?
 *
 * It can be shown empirically that most unreachable objects become unreachable while they 
 * are still young. Generational gc algorithms take this empirical fact into account and
 * divide the objects into generations, whereby the (small) youngest generation of objects is
 * garbage-collected more frequently.
 *
 * A typical problem of the simple gc algorithms we discussed is the stop-the-world phenomenon:
 * All execution has to be stopped during a gc cycle.
 * This issue is addressed by incremental or concurrent garbage collectors. Incremental garbage collectors
 * typically reduce the total throughput but increase responsiveness and real-time behavior. 
 *
 * A completely different approach to memory management is _reference counting_.  In reference counting,
 * each object on the heap (in our case, each box) maintains a counter which says how many pointers
 * currently point to that object. The counter is adjusted whenever a pointer variable is assigned to
 * this object (incremented), or from this object to another object (decremented). When the counter is 0,
 * the object can be reclaimed.
 *
 * The obvious disadvantage of reference counting is that it cannot detect cycles on the heap. Hence
 * reference counting algorithms must be augmented with some means to detect cycles. 
 *
 * Topic for class discussion: What are the pros and cons of reference counting vs tracing garbage 
 * collectors such as mark-and-sweep or semi-space?
 */
 
                      
                      