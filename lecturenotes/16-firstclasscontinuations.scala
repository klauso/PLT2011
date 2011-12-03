/*
These are lecture notes for the "Programming Languages and Types" course by Klaus Ostermann 
at the University of Marburg

loosely based on Sec. 18 of "Programming Languages: Application and Interpretation" by
Shriram Krishnamurthi

Please comment/correct/improve these notes via github. Proposals or questions can
be submitted as an "issue"; proposals for corrections/extensions/improvements can
be submitted as a "pull request". You can of course also send an email to Klaus Ostermann */

/* Today's goal is to formalize first-class continuations as illustrated by Scheme's let/cc 
 * construct. In the previous lecture we have learned why first class continuations are
 * a powerful language construct. Today we learn the semantics of first-class continuations
 * by extending our interpreter to support letcc. Here is the abstract syntax of the
 * language we want to extend: */

sealed abstract class Exp
case class Num(n: Int) extends Exp
case class Id(name: Symbol) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Fun(param: Symbol, body: Exp) extends Exp
implicit def num2exp(n: Int) = Num(n)
implicit def id2exp(s: Symbol) = Id(s)
case class App (funExpr: Exp, argExpr: Exp) extends Exp

/* The abstract syntax of Letcc is as follows: */
case class Letcc(param: Symbol, body: Exp) extends Exp

/*
 * But how do we implement letcc? How do we get hold of the rest of the computation of the
 * object (=interpreted) program? 
 * 
 * One idea would be to CPS-transform the object program. Then we have the current continuation
 * available and could store it in environments etc.
 *
 * However, we want to give a direct semantics to first-class continuations, without first 
 * transforming the object program. 
 *
 * Insight: If we would CPS-transform the interpreter, the continuation of the interpreter also
 * represents, in some way, the continuation of the object program. The difference is that
 * it represents what's left to do in the interpreter and not in the object program. However,
 * what is left in the interpreter _is_ what is left in the object program.
 *
 * Hence we are faced with two tasks:
 * 1) CPS-transform the interpreter
 * 2) add a branch for Letcc to the interpreter.
 *
 * Let's start with the standard infrastructure of values and environments. */

sealed abstract class Value
type Env = Map[Symbol, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value

/* How do we represent values that represent continuations? Since we want to
 * represent an object language continuation by a meta language continuation,
 * we need to be able to wrap a meta language continuation as an object language
 * value. This continuation will always accept some other object language value: */

case class ContV(f: Value => Nothing) extends Value

/* We also need a syntactic construct to apply continuations. One way to provide
 * such a construct would be to add a new syntactic category of continuation application.
 * We will instead do what Scheme and other languages also do: We overload the
 * normal function applicaton construct and also use it for application of continuations.
 * This means that we will need a case distinction in our interpreter whether the function
 * argument is a closure or a continuation. */

/* Let's now study the interpreter for our new language. The branches for
 * Num, Id, Add, and Fun are straightforward applications of the CPS
 * transformation technique we already know. */ 

 def eval(e: Exp, env: Env, k: Value => Nothing) : Nothing = e match {
  case Num(n: Int) => k(NumV(n))
  case Id(x) => k(env(x))
  case Add(l,r) => {
    eval(l,env, lv => 
        eval(r,env, rv =>
          (lv,rv) match {
            case (NumV(v1), NumV(v2)) => k(NumV(v1+v2))
            case _ => sys.error("can only add numbers")
          }))
  }
  case f@Fun(param,body) => k(ClosureV(f, env))
  
  /* In the application case we now need to distinguish whether the first argument
   * is a closure or a continuation. If it is a continuation, we ignore the
   * current continuation k and "jump" to the stored continuation by applying the
   * evaluated continuation argument to it. */
  case App(f,a) => eval(f,env, cl => cl match {
            case ClosureV(f,closureEnv) => eval(a,env, av => eval(f.body, closureEnv + (f.param -> av),k))
            case ContV(f) => eval(a,env, av => f(av))
            case _ => sys.error("can only apply functions")
  })
  /* Letcc is now surprisingly simple: We continue the evaluation in the body in an
   * extended environment in which param is bound to the current continuation k,
   * wrapped as a value using ContV. */
  case Letcc(param,body) => eval(body, env+(param -> ContV(k)), k)  
}

/* To make it easier to experiment with the interpreter this code provides the
 * right initialization to eval. We have to give eval a continuation which represents
 * the rest of the computation after eval is done. A small technical problem arises due to our 
 * usage of the return type "Nothing" for continuations, to emphasize that they
 * do not return: The only way to implement a value that has this type is a
 * function that does indeed not return. We do so by letting this function
 * throw an exception. To keep track of the returned value we store it temporarily
 * in a variable, catch the exception, and return the stored value. */
 
def starteval(e: Exp) : Value = {
  var res : Value = null
  val s : Value => Nothing = (v) => { res = v; sys.error("program terminated") }
  try { eval(e, Map.empty, s) } catch { case e:Throwable => Unit }
  res
}

/* Finally a small test of Letcc. */
val testprog = Add(1, Letcc('k, Add(2, App('k, 3))))

assert(starteval(testprog) == NumV(4))
