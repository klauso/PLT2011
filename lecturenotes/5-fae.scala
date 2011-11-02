/*
These are lecture notes for the "Programming Languages and Types" course by Klaus Ostermann 
at the University of Marburg

loosely based on Sec. 6 of "Programming Languages: Application and Interpretation" by
Shriram Krishnamurthi

Please comment/correct/improve these notes via github. Proposals or questions can
be submitted as an "issue"; proposals for corrections/extensions/improvements can
be submitted as a "pull request". You can of course also send an email to Klaus Ostermann */


/* F1-WAE, the language with first-order functions, lets us abstract over patterns
 * that involve numbers. But what if we want to abstract over patterns that
 * involve functions, such as the "list fold" pattern, whose instantiations
 * include summing or multiplying a list of integers?
 * 
 * To enable this kind of abstraction, we need to make functions "first-class",
 * which means that they become values that can be passed to or returned from
 * functions or stored in data structures. Languages with first-class functions
 * enable so-called "higher-order functions", which are functions that accept or
 * return a (possibly again higher-order) function.
 *
 * We will see that this extension will make our language both simpler and much
 * more powerful. This seeming contradiction is famously addressed by the first
 * sentence of the Scheme language specification:
 *
 *  "Programming languages should be designed not by piling feature on top of 
 *   feature, but by removing the weaknesses and restrictions that make additional 
 *   features appear necessary."
 * 
 * The simplicity is due to the fact that this language is so expressive that many
 * other language features can be "encoded", i.e., they do not need to be added
 * to the language but can be expressed with the existing features.
 *
 * This language, which we call "FAE", is basically the so-called "lambda calculus",
 * a minimal but powerful programming language that has been highly influential
 * in the design and theory of programming languages. 
 *
 * FAE is the language of arithmetic expressions, AE, plus only two additional
 * language constructs: Function definition and function application. 
 */

 
sealed abstract class Exp
case class Num(n: Int) extends Exp
case class Id(name: Symbol) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
implicit def num2exp(n: Int) = Num(n)
implicit def id2exp(s: Symbol) = Id(s)

/* Both function definitions and applications are expressions. */ 
case class Fun(param: Symbol, body: Exp) extends Exp
case class App (funExpr: Exp, argExpr: Exp) extends Exp

/* Due to the lambda calculus, the concrete syntax for function definition is
 * often written with a lambda, such as "lambda x. x+3". The Scala syntax
 * for lambda terms is " (x) => x+3", the Haskell syntax is "\x -> x+3".
 * 
 * The concrete syntax for function application is often either 
 * juxtaposition "f a" or using brackets "f(a)". Haskell and the lambda calculus
 * use the former, Scala uses the latter.
 *
 * The "with" construct is not needed anymore since it can be encoded
 * using App and Fun. For instance, "with x = 7 in x+3" can be encoded
 * (using Scala syntax) as " ((x) => x+3)(7)
 *
 * We make this idea explicit by giving a constructive translation. Such translations
 * are also often called "desugaring".
 */
 
 // "with" would be a better name for this function, but it is reserved in Scala
def wth(x: Symbol, xdef: Exp, body: Exp) : Exp = App(Fun(x,body),xdef)

/* Like for F1WAE, we will at first define the meaning of FAE in terms of
 * substitution. Here is the substitution function for FAE. */
 
def subst(e1 : Exp, x: Symbol, e2: Exp) : Exp = e1 match {
  case Num(n) => e1
  case Add(l,r) => Add(subst(l,x,e2), subst(r,x,e2))
  case Id(y) => if (x == y) e2 else Id(y)
  case App(f,a) => App(subst(f,x,e2),subst(a,x,e2))
  case Fun(param,body) => 
    if (param == x) e1  else Fun(param, subst(body, x, e2))
}

/* Let's try whether subst produces reasonable results. */
assert( subst(Add(5,'x), 'x, 7) == Add(5, 7))
assert( subst(Add(5,'x), 'y, 7) == Add(5,'x))
assert( subst(Fun('x, Add('x,'y)), 'x, 7) == Fun('x, Add('x,'y)))

/* However, what happens if e2 contains free variables? The danger
 * here is that they may be accidentially "captured" by the substitution.
 * For instance, consider 
 *
 *  subst(Fun('x, Add('x,'y)), 'y, Add('x,5))
 *
 * The result is Fun('x,Add('x,Add('x,5)))
 * 
 * This is not desirable, since it violates again static scoping.
 *
 * Note that this problem did not show up in earlier languages, because
 * there we only substituted variables by numbers, but not by expressions
 * that may contain free variables: The type of e2 was "Num" and not "Exp".
 *
 * Hence we are still not done with defining substitution.
 * But what is the desired result of the substitution above?
 * The answer is that we must avoid the name clash by renaming the variable
 * bound by the "lambda" if the variable name occurs free in e2. This new
 * variable name should be "fresh", i.e., not occur free in e2.
 * 
 * For instance, in the example above, we could first rename 'x to the fresh name
 * 'x0 and only then substitute, i.e. 
 *
 *   subst(Fun('x, Add('x,'y)), 'y, Add('x,5)) == Fun('x0,Add(Id('x0),Add(Id('x),Num(5))))
 *
 * Let's do this step by step.
 */
 
def freshName(names: Set[Symbol], default: Symbol) : Symbol = {
  var last : Int = 0
  var freshName = default  
  while (names contains freshName) { freshName = Symbol(default.name+last.toString); last += 1; }
  freshName
}

assert( freshName(Set('y,'z),'x) == 'x)
assert( freshName(Set('x2,'x0,'x4,'x,'x1),'x) == 'x3)

def freeVars(e: Exp) : Set[Symbol] =  e match {
   case Id(x) => Set(x)
   case Add(l,r) => freeVars(l) ++ freeVars(r)
   case Fun(x,body) => freeVars(body) - x
   case App(f,a) => freeVars(f) ++ freeVars(a)
   case Num(n) => Set.empty
}
assert(freeVars(Fun('x,Add('x,'y))) == Set('y))

def subst(e1 : Exp, x: Symbol, e2: Exp) : Exp = e1 match {
  case Num(n) => e1
  case Add(l,r) => Add(subst(l,x,e2), subst(r,x,e2))
  case Id(y) => if (x == y) e2 else Id(y)
  case App(f,a) => App(subst(f,x,e2),subst(a,x,e2))
  case Fun(param,body) => 
    if (param == x) e1 else {
      val newvar = freshName(freeVars(e2), param)
      Fun(newvar, subst(subst(body, param, Id(newvar)), x, e2))
    }                            
}

assert( subst(Add(5,'x), 'x, 7) == Add(5, 7))
assert( subst(Add(5,'x), 'y, 7) == Add(5,'x))
assert( subst(Fun('x, Add('x,'y)), 'x, 7) == Fun('x, Add('x,'y)))
// test capture-avoiding substitution
assert( subst(Fun('x, Add('x,'y)), 'y, Add('x,5)) == Fun('x0,Add(Id('x0),Add(Id('x),Num(5)))))

/* OK, equipped with this new version of substitution we can now define the interpreter
 * for this language.
 *
 * But how do we evaluate a function definition? Obviously we cannot return a number.
 *
 * We realize that functions are also values! Hence we have to broaden the return type of
 * our evaluator to also allow functions as values. 
 * For simplicity, we use "Exp" as our return type since it allows us to return both
 * numbers and functions. Later we will become more sophisticated.
 *
 * This means that a new class of errors can occur: A subexpression evaluates to a number
 * where a function is expected, or vice versa. Such errors are called _type errors_, and
 * we will talk about them in much more detail later. For now, it means that we need to
 * analyze (typically by pattern matching) the result of recursive invocations of eval to
 * check whether the result has the right type.
 *
 * The remainder of the interpreter is unsurprising. 
 */
def eval(e: Exp) : Exp = e match {
  case Id(v) => sys.error("unbound identifier: "+v)
  case Add(l,r) => (eval(l), eval(r)) match {
                     case (Num(x),Num(y)) => Num(x+y)
                     case _ => sys.error("can only add numbers")
                    }
  case App(f,a) => eval(f) match {
     case Fun(x,body) => eval( subst(body,x, eval(a)))
     case _ => sys.error("can only apply functions")
  }
  case _ => e // numbers and functions evaluate to themselves
}

/* We can also make the return type more precise to verify the invariant 
 * that numbers and functions are the only values. */
def eval2(e: Exp) : Either[Num,Fun] = e match {
  case Id(v) => sys.error("unbound identifier: "+v)
  case Add(l,r) => (eval2(l), eval2(r)) match {
                     case (Left(Num(x)),Left(Num(y))) => Left(Num(x+y))
                     case _ => sys.error("can only add numbers")
                    }
  case App(f,a) => eval2(f) match {
     case Right(Fun(x,body)) => eval2( subst(body,x, eval(a)))
     case _ => sys.error("can only apply functions")
  }
  case f@Fun(_,_) => Right(f) 
  case n@Num(_) => Left(n)
}

/* Let's test. 
 * Exercise: Add more interesting test cases.
 */
val test = App( Fun('x,Add('x,5)), 7)

assert( eval(test) == Num(12))

/* FAE is a computationally (Turing)-complete language. For instance, we can define 
 * a non-terminating program. This program is commonly called Omega */
 
val omega = App(Fun('x,App('x,'x)), Fun('x,App('x,'x)))

// try eval(omega) to crash the interpreter ;-)

/* Omega can be extended to yield a fixed point combinator, which can be
 * used to encode arbitrary recursive functions. We come back to this topic
 * later. */

/* Let's now discuss what an environment-based version of this interpreter looks like.
 *
 * Here is a first attempt.
 */
type Env0 = Map[Symbol, Exp]

def evalWithEnv0(e: Exp, env: Env0) : Exp = e match {
  case Id(x) => env(x)
  case Add(l,r) => {
    (evalWithEnv0(l,env), evalWithEnv0(r,env)) match {
      case (Num(v1),Num(v2)) => Num(v1+v2)
      case _ => sys.error("can only add numbers")
    }
  }
  case App(f,a) => evalWithEnv0(f,env) match {
    case Fun(f,body) => evalWithEnv0(body, Map(f -> evalWithEnv0(a,env)))
    case _ => sys.error("can only apply functions")
  }
  case _ => e // numbers and functions evaluate to themselves 
}

assert( evalWithEnv0(test, Map.empty) == Num(12))

/* However, consider the following example. */ 
val test2 = wth('x, 5, App(Fun('f, App('f,3)), Fun('y,Add('x,'y))))

/* It works fine in the substitution-based interpreter. */
assert(eval(test2) == Num(8))

/* but evalWithEnv0(test2,Map.empty) yields an "identifier not found: 'x" error. 
 * 
 * What can we do to fix this problem?
 *
 * The problem is that we have forgotten the deferred substitutions to be performed
 * in the body of the function.
 *
 * We could try to replace the second line in the "App" case by
 *     case Fun(f,body) => evalWithEnv0(body, env + (f -> evalWithEnv0(a,env)))
 * but this would again introduce dynamic scoping.
 *
 * Hence, when we evaluate a function, we do not only have to store the function, but also
 * the environment active when the function was defined. This pair of function and environment
 * is called a _closure_. The environment stored in the closure is used when the function is
 * eventually applied.
 *
 * Hint: If you cannot answer what a closure is and how it is used in the 
 * interpreter, you will be toast in the exam!
 *
 * Since closures are not expressible in the language syntax, we now come to the point where
 * we need a separate category of _values_. The values in FAE can be either numbers or closures. 
 */
 
sealed abstract class Value
type Env = Map[Symbol, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value

/* The evaluator becomes :*/

def evalWithEnv(e: Exp, env: Env) : Value = e match {
  case Num(n: Int) => NumV(n)
  case Id(x) => env(x)
  case Add(l,r) => {
    (evalWithEnv(l,env), evalWithEnv(r,env)) match {
      case (NumV(v1),NumV(v2)) => NumV(v1+v2)
      case _ => sys.error("can only add numbers")
    }
  }
  case f@Fun(param,body) => ClosureV(f, env)
  case App(f,a) => evalWithEnv(f,env) match {
    // Use environment stored in closure to realize proper lexical scoping!
    case ClosureV(f,closureEnv) => evalWithEnv(f.body, closureEnv + (f.param -> evalWithEnv(a,env)))
    case _ => sys.error("can only apply functions")
  }
}
assert( evalWithEnv(test, Map.empty) == NumV(12))
assert( evalWithEnv(test2,Map.empty) == NumV(8))