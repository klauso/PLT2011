/*
These are lecture notes for the "Programming Languages and Types" course by Klaus Ostermann 
at the University of Marburg

Please comment/correct/improve these notes via github. Proposals or questions can
be submitted as an "issue"; proposals for corrections/extensions/improvements can
be submitted as a "pull request". You can of course also send an email to Klaus Ostermann */

/* Today we shrink our language. It does not seem to be big, but today we want to 
 * illustrate how powerful our core language, the lambda calculus, is.
 * Here is a shrinked version of FAE that does not even have numbers anymore.
 * For testing purposes, we introduce a new expression "PrintDot" whose semantics is
 * to print a dot on the screen. 
 */
sealed abstract class Exp
case class Id(name: Symbol) extends Exp
implicit def id2exp(s: Symbol) = Id(s)
case class Fun(param: Symbol, body: Exp) extends Exp
case class App (funExpr: Exp, argExpr: Exp) extends Exp
case class PrintDot() extends Exp

abstract class Value // the only values are closures
type Env = Map[Symbol, Value]
case class ClosureV(f:Fun, env:Env) extends Value

/* Notice that the only values in this language are closures.
 * This means that there cannot be the situation anymore that
 * we expect, say, a number but get in fact a closure.
 * Hence, this language has the fascinating property that 
 * no dynamic type errors can occur. */
 
def eval(e: Exp, env: Env) : Value = e match {
  // We give the identity function as dummy value for PrintDot
  case PrintDot() => print("."); ClosureV(Fun('x,'x), Map.empty) 
  case Id(x) => env(x)
  case f@Fun(param,body) => ClosureV(f, env)
  case App(f,a) => eval(f,env) match {
    case ClosureV(f,closureEnv) => eval(f.body, closureEnv + (f.param -> eval(a,env)))
  }
}

/* Now we want to illustrate that we can, in principle, bootstrap a full
 * programming language from this small core. To do so, we use the technique
 * of Church encoding, which you already learnt in the first lecture, this
 * time using pure functions. This means that each datum is represented by
 * its own fold function.
 *
 * Let's start with booleans and boolean arithmetic. */
val f = Fun('t, Fun('f, 'f))  // false
val t = Fun('t, Fun('f, 't))  // true
val and = Fun('a, Fun('b, App(App('a, 'b),'a)))
val or = Fun('a, Fun('b, App(App('a, 'a), 'b)))
val not = Fun('a, Fun('t, Fun('f, App(App('a,'f),'t))))

/* We can now write our own control structures, such as if/then/else */
val ifthenelse = Fun('cond, Fun('t, Fun('f, App(App('cond, 't), 'f))))

/* Let's now consider Numbers. We encode them as Peano numbers. 
 * These encodings of numbers are often called "Church numerals". */
val zero = Fun('s, Fun('z, 'z))
val succ = Fun('n, Fun('s, Fun('z, App('s, App(App('n, 's),'z)))))
val one = App(succ, zero)
val two = App(succ, one)
val three = App(succ, two)
val add  = Fun('a, Fun('b, Fun('s, Fun('z, App(App('a,'s), App(App('b, 's),'z))))))
val mult = Fun('a, Fun('b, Fun('s, Fun('z, App(App('a, App('b,'s)), 'z)))))
val exp  = Fun('a, Fun('b, App(App('b, App(mult, 'a)), one)))
val iszero = Fun('a, App(App('a, Fun('x, f)), t))
/* The predecessor function is more complicated (why?). We do not show it here. */

/* For testing, here is a function that prints a unary representation of a number. */
val printnum = Fun('a, App(App('a, Fun('x, PrintDot())), f))

/* Church encoding of lists. Again straightforward, except "tail", which we do not
 * show here. It needs the same kind of trick (called "pairing trick") as the predecessor
 * function. */
val emptylist = Fun('c, Fun('e, 'e))
val cons = Fun('h, Fun('r, Fun('c, Fun('e, App(App('c, 'h), App(App('r,'c),'e))))))
val head = Fun('l, App(App('l, Fun('h, Fun('t, 'h))), f))

/* For instance, we can multiply all numbers in a list */
val multlist = Fun('l, App(App('l, mult), one))
/* Here is the list 3,2,3 */
val list323 = App(App(cons, three), App(App(cons, two), App(App(cons,three),emptylist)))

val test = App(printnum, App(multlist, list323))
/* Calling exec should yield 18 dots before the dummy result */
def exec = eval(test, Map.empty) 

/* Topic for class discussion: Can we do these encodings directly in Scala or Haskell? */