/*
These are lecture notes for the "Programming Languages and Types" course by Klaus Ostermann 
at the University of Marburg

loosely based on Sec. 9+10 of "Programming Languages: Application and Interpretation" by
Shriram Krishnamurthi

Please comment/correct/improve these notes via github. Proposals or questions can
be submitted as an "issue"; proposals for corrections/extensions/improvements can
be submitted as a "pull request". You can of course also send an email to Klaus Ostermann */

/* =========
 * Recursion
 * =========
 * 
 * Let's try to write a function that computes the sum of the first n integers.
 * Let's pretend we do not know that the sum of the first n integers is n*(n+1)/2
 * and instead compute the sum in a loop. Let's try to do this in FAE (with if0):
 */
 
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

val sumattempt = wth('sum, Fun('n, If0('n, 0, Add('n, App('sum, Add('n,-1))))), App('sum, 10))

/* However, sumattempt won't work and yield an unbound identifier error (why?).
 * An alternative would be to use a variant of the y combinator to support recursion
 * properly, but today we want to talk about direct support for recursion.
 * More specifically, we want a language construct "letrec" that is similar to "with", 
 * except that the bound symbol can be used in the expression the symbol is bound to:
 */
case class Letrec(x: Symbol, e: Exp, body: Exp) extends Exp

/* Using letrec, our example can be expressed as follows. */

val sum = Letrec('sum, Fun('n, If0('n, 0, Add('n, App('sum, Add('n,-1))))), App('sum, 10))

/* Let's now consider the semantics of letrec. Consider the evaluation of
 * Letrec(x,e,body) in an environment env.
 *
 * What environment should we use to evaluate e and body, respectively?
 * Using env for e will produce a ClosureV(Fun('n,...'sum'...),env), and hence
 * the environment when evaluating body will be
 * envbody = env + (x -> ClosureV(Fun('n,...'sum...),env))
 * This is bad, because the env in the closure does not contain a binding for sum and hence the
 * recursive invocation will fail. The environment in the closure must contain
 * a mapping for 'sum. Hence envbody should look like
 * envbody = env + (x -> ClosureV(Fun('n, ...'sum...), 
 *                                env+('sum -> ClosureV(Fun('n,...'sum...),env)))
 * 
 * This looks better, but now the second closure contains an environment with no
 * binding of 'sum. What we need is an environment that satisfies the equation:
 *
 * envbody = env + (x -> ClosureV(Fun('n, ...'sum..), envbody))
 *
 * Obviously envbody must be circular. There are different ways to create such a circular
 * environment. We will choose mutation to create a cycle. More specifically, we use
 * a mutable Map (initialized to env) as environment when evaluating e and then mutate
 * the environment to include a mapping from x to the evaluated closure.
 *
 * To be able to use both mutable and immutable maps as environments, we define: 
 */


/* No changes to the values in our language. */
sealed abstract class Value

type Env = scala.collection.Map[Symbol, Value] // just "Map" defaults to scala.collection.immutable.Map

case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value


/* The interpreter is unchanged except for the additional Letrec case. */
def eval(e: Exp, env: Env) : Value = e match {
  case Num(n: Int) => NumV(n)
  case Id(x) => env(x)
  case If0(cond, thenExp, elseExp) => eval(cond,env) match {
    case NumV(0) => eval(thenExp,env)
    case _ => eval(elseExp,env)
  }    
  case Add(l,r) => {
    (eval(l,env), eval(r,env)) match {
      case (NumV(v1),NumV(v2)) => NumV(v1+v2)
      case _ => sys.error("can only add numbers")
    }
  }
  case f@Fun(param,body) => ClosureV(f, env)
  case App(f,a) => eval(f,env) match {
    case ClosureV(f,closureEnv) => eval(f.body, closureEnv + (f.param -> eval(a,env)))
    case _ => sys.error("can only apply functions")
  }
  case Letrec(x,e,body) => {
    val mutableenv =  scala.collection.mutable.Map() ++ env // create mutable map, initialize it to env
    mutableenv += x -> eval(e,mutableenv)  // evaluate e and then create cycle in the environment
    eval(body,mutableenv) // evaluate body in cyclic environment
  }
}

/* The sum of numbers from 1 to 10 should be 55. */
assert(eval(sum, Map.empty) == NumV(55))
