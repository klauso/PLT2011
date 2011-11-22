/*
These are lecture notes for the "Programming Languages and Types" course by Klaus Ostermann 
at the University of Marburg

Please comment/correct/improve these notes via github. Proposals or questions can
be submitted as an "issue"; proposals for corrections/extensions/improvements can
be submitted as a "pull request". You can of course also send an email to Klaus Ostermann */


/* For each desired language semantics, there exist many different ways to
 * implement an interpreter in some meta-language to encode this semantics.
 *
 * One question that is of particular importance is whether a language feature
 * is implemented by using a corresponding language feature of the meta-language,
 * or whether it is implemented using more primitive language constructs.
 * The first case is called meta-interpretation, the second case syntactic interpretation. 
 *
 * Meta-interpretation can be convenient if our purpose if we are not interested in having
 * control over the exact meaning of the construct, or if the way the meta-language handles
 * this construct is just what we want for our object language. 
 * Syntactic interpretation is required if we want to understand what the language feature
 * really means in terms of more primitive constructs, or if we want to implement the
 * language feature differently than the meta language. 
 * Of course, if the meta language has no corresponding feature, then we have no choice
 * but to make a syntactic interpretation.
 *
 * Our FAE interpreter is a meta-interpreter with respect to many features. For instance,
 * it does not tell us
 * - the precision of numbers, or the algorithm for addition
 * - how the call stack is managed, e.g. the maximum depth of recursion supported by the interpreter
 * - whether/how memory management for closures works (they are objects on the heap!)
 *
 * That said, it is possible to make the FAE interpreters still more "meta".
 * Here are two examples.
 * Here is a version of FAE that uses a different representation of the program syntax,
 * namely one using meta-language functions to represent object-language functions.
 * This technique is called higher-order abstract syntax, or HOAS.
 * For instance, the function Fun('x, Add('x,5)), is now represented as Fun( x => Add(x,5)).
 *
 * The interpreter becomes rather short, because substitution and lexical scoping are
 * now being dealt with by the corresponding meta-level construct.
 */
object HOAS {
    sealed abstract class Exp
    case class Num(n: Int) extends Exp
    case class Id(name: Symbol) extends Exp
    case class Add(lhs: Exp, rhs: Exp) extends Exp
    case class Fun(f: Exp => Exp) extends Exp
    case class App (funExpr: Exp, argExpr: Exp) extends Exp
    def eval(e: Exp) : Exp = e match {
      case Id(v) => sys.error("unbound identifier: "+v)
      case Add(l,r) => (eval(l), eval(r)) match {
                         case (Num(x),Num(y)) => Num(x+y)
                         case _ => sys.error("can only add numbers")
                        }
      case App(f,a) => eval(f) match {
         case Fun(f) => eval( f(eval(a)))
         case _ => sys.error("can only apply functions")
      }
      case _ => e // numbers and functions evaluate to themselves
    }      
}

/* A different way to use meta-level functions in the interpreter is
 * to represent object-level closures by meta-level closures.
 * Notice that this interpreter has no control anymore about scoping; 
 * rather, it is completely inherited from the meta language. 
 *
 * A particularly pleasing and important property of this interpreter
 * is that it is _compositional_, meaning that all recursive calls of eval
 * are only on subparts of the original expression. This means that it
 * becomes particularly easy to reason about program equivalence in the
 * object language in terms of program equivalence in the meta language:
 * Two object language expressions are equivalent if their "denotations"
 * as meta-level expressions are equivalent in the meta-level. 
 *
 * Compositionality is the cornerstone of denotational semantics. 
 * A denotational semantics can be thought of as a compositional interpreter
 * in which the meta-language is mathematics.
 *
 * Compositionality also has a rather practical side-effect: It means that
 * we can implement the interpreter in the internal visitor style that 
 * we learned about in the first lecture (recall that the internal visitor 
 * style enforces compositionality)
 *
 * Recommended exercise: Re-implement the interpreter as an internal visitor. 
 */
sealed abstract class Exp
case class Num(n: Int) extends Exp
case class Id(name: Symbol) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Fun(param: Symbol, body: Exp) extends Exp
case class App (funExpr: Exp, argExpr: Exp) extends Exp

object Compositional { 
    sealed abstract class Value
    type Env = Map[Symbol, Value]
    case class NumV(n: Int) extends Value
    case class FunV(f: Value => Value) extends Value

    def eval(e: Exp) : Env => Value = e match {
      case Num(n: Int) => (env) => NumV(n)
      case Id(x) => env => env(x)
      case Add(l,r) => { (env) =>
        (eval(l)(env),  eval(r)(env)) match {
          case (NumV(v1),NumV(v2)) => NumV(v1+v2)
          case _ => sys.error("can only add numbers")
        }
      }
      case Fun(param,body) => (env) => FunV( (v) => eval(body)(env + (param -> v)))
      case App(f,a) => (env) => (eval(f)(env), eval(a)(env)) match {
        // Use environment stored in closure to realize proper lexical scoping!
        case (FunV(g),arg) => g(arg)
        case _ => sys.error("can only apply functions")
      }
    }
}
/* For comparison, here is our original FAE interpreter. */
object FAE {
    sealed abstract class Value
    type Env = Map[Symbol, Value]
    case class NumV(n: Int) extends Value
    case class ClosureV(f: Fun, env: Env) extends Value

    def eval(e: Exp, env: Env) : Value = e match {
      case Num(n: Int) => NumV(n)
      case Id(x) => env(x)
      case Add(l,r) => {
        (eval(l,env), eval(r,env)) match {
          case (NumV(v1),NumV(v2)) => NumV(v1+v2)
          case _ => sys.error("can only add numbers")
        }
      }
      case f@Fun(param,body) => ClosureV(f, env)
      case App(f,a) => eval(f,env) match {
        // Use environment stored in closure to realize proper lexical scoping!
        case ClosureV(f,closureEnv) => eval(f.body, closureEnv + (f.param -> eval(a,env)))
        case _ => sys.error("can only apply functions")
      }
    }
}

/* We will soon learn about ways to make FAE more syntactic in various ways.
 * For instance, we will no longer rely on call-stack management of the meta-language,
 * or the existence of higher-order functions.
 *
 * One dimension in which the interpreter could easily be made more syntactic is
 * the treatment of numbers and arithmetic. For instance, we could represent numbers
 * as sequences of digits instead of Scala numbers. 
 * 
 * Another aspect in which our FAE interpreter relies on the host language is memory
 * management. This is particularly relevant for environments stored inside closures.
 * These environments cannot be organized on the call stack and hence need memory
 * management. Since we are using Scala references to refer to environments, environments
 * that are no longer needed are collected by the Scala (or rather, Java) virtual machine.
 */