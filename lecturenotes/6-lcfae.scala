/*
These are lecture notes for the "Programming Languages and Types" course by Klaus Ostermann 
at the University of Marburg

loosely based on Sec. 6 of "Programming Languages: Application and Interpretation" by
Shriram Krishnamurthi

Please comment/correct/improve these notes via github. Proposals or questions can
be submitted as an "issue"; proposals for corrections/extensions/improvements can
be submitted as a "pull request". You can of course also send an email to Klaus Ostermann */

/* Lazy Evaluation
 * ===============
 *
 * Motivation for Lazy Evaluation
 * ------------------------------
 * read Chap. 7 of PLAI and get acquainted with basic Haskell
 * (install GHC from www.haskell.org and browse through one of the tutorials)
 *
 * read "Why Functional Programming Matters" by John Hughes
 * available at http://www.cse.chalmers.se/~rjmh/Papers/whyfp.html
 *  
 * What lazy evaluation means
 * --------------------------
 * The choice of evaluation strategy is a purely semantic change that requires
 * no change to the syntax.
 * For this reason we reuse the syntactic definitions of FAE, hence
:load 5-fae.scala
 * before executing this script or uncomment the previous line.
 *
 * Before we discuss lazy evaluation, we will first discuss a a
 * related evaluation strategy, call-by-name.
 *
 * Call-by-name can be explained very succintly in the substitution-based
 * interpreter: Instead of evaluating the argument "a" in the "App" case
 * before substitution, we substitute the unevaluated argument into the body.
 * The rest remains exactly the same. */

def evalcbn(e: Exp) : Exp = e match {
  case Id(v) => sys.error("unbound identifier: "+v)
  case Add(l,r) => (evalcbn(l), evalcbn(r)) match {
                     case (Num(x),Num(y)) => Num(x+y)
                     case _ => sys.error("can only add numbers")
                    }
  case App(f,a) => evalcbn(f) match {
     case Fun(x,body) => evalcbn( subst(body,x, a)) // no evaluation of a!
     case _ => sys.error("can only apply functions")
  }
  case _ => e 
}

/* Does this change the semantics, or is it just an implementation detail?
 * In other words, is eval(e) == evalcbn(e) for all e ?
 * Let's try two former test cases.
 */
 
assert( evalcbn(test) == eval(test))
assert(evalcbn(test2) == eval(test2))

/* One can formally prove that if eval and evalcbn both produce a number 
 * then the numbers are equal. Do they also agree if they produce a function?
 *
 * Not necessarily. Consider: */

val test3 = App(Fun('f,Fun('x,App('f,'x))),Add(1,2)) 

assert(eval(test3) == Fun('x,App(Num(3),Id('x))))
assert(evalcbn(test3) == Fun('x,App(Add(Num(1),Num(2)),Id('x))))

/* However, if both produce a function, then the functions "behave" the same. 
 * More specifically, the function bodies produced by evalcbn may be 
 * "more evaluated" than those produced by eval. If we would evaluate within
 * function bodies (also called evaluation "under a lambda") - which our interpreters
 * do not do - we could produce the expression returned from eval from the expression
 * returned by evalcbn. This kind of equivalence is also called "beta-equivalence".
 *
 * Most importantly, however, eval and evalcbn differ with regard to their termination
 * behavior. We have seen that omega is a diverging expression. In eval, the term */

 val test4 = App(Fun('x,5),omega)

/* is hence also diverging. In contrast: */

assert(evalcbn(test4) == Num(5))

/* Extra material: Infinite lists in FAE (not relevant for exam) 
 * ------------------------------------------------------
 * Using our call-by-name interpreter, we can express the same kinds of programming
 * patterns that we have tried in Haskell, such as infinite lists.
 *
 * We do not have direct support for lists, but luckily we already know
 * how to Church-encode them (see also lab-01.scala). Church-encoding can also
 * be done in FAE. Here is the one for lists: */
val nil = Fun('c, Fun('e, 'e))
val cons  = Fun('x, Fun('xs, Fun('c, Fun('e, App(App('c, 'x), App(App('xs, 'c),'e))))))
/* For instance, the list 1,2,3 is encoded as: */
val list123 = App(App('cons,1),App(App('cons,2),App(App('cons,3), 'nil)))
/* The map function on lists becomes :*/
val maplist = Fun('f, Fun('l, App(App('l, Fun('x, Fun('xs, App(App('cons, App('f,'x)),'xs)))), 'nil)))
/* For instance, we can map the successor function over the 1,2,3 list. */
val test5 = wth('cons,cons, 
            wth('nil, nil, 
            wth('maplist, maplist,
            App(App('maplist, Fun('x, Add('x,1))), list123))))
/* Since it is somewhat difficult to print out the resulting list in our primitive language
   we construct the result we expect explicitly. */
val test5res = wth('cons,cons, 
               wth('nil, nil, 
                 App(App('cons,2),App(App('cons,3),App(App('cons,4), 'nil)))))
assert(eval(test5) == eval(test5res))     
/* Using evalcbn instead of eval the assertion does not hold (why?), but the results are beta-equivalent. */           

/* We can also construct infinite lists. To this end, we need some form of recursion. We choose 
 * the standard fixed-point operator Y. This operator only works under call-by-name or other 
 * so-called "non-strict" evaluation strategies. */            
val y = Fun('f, App(Fun('x,App('f, App('x,'x))), Fun('x,App('f,App('x,'x)))))
/* Using Y, we can construct infinite lists, such as the list of all natural numbers. */ 
val allnats = App(App('y, Fun('nats, Fun('n, App(App('cons,'n), App('nats, Add('n,1)))))),1)
/* We can also perform standard computations on infinite lists, such as mapping the successor function over it. */
val list2toinfty = wth('cons,cons, 
                   wth('nil, nil, 
                   wth('y, y, 
                   wth('maplist, maplist,
                      App(App('maplist, Fun('x, Add('x,1))), allnats)))))
/* Of course, list2toinfty diverges when we use eval, but it works fine with evalcbn. It is hard to verify the result
 * due to an almost unreadable output. Hence we propose the following 
 *
 * Exercise: Extend the language such that you can implement the "take" function as known from Haskell within 
 *           the language  (if0-expressions or something like it are needed for that)
 *           Now add a "print" function that prints a number on the console. Use it to display the first 
 *           3 list elements of test2toinfty are 2,3,4. 
 *
 * -- end of extra material --
 */
 
/* Let us now consider the question how we can implement call-by-name in the environment-based interpreter.
 * Translating the idea of not evaluating the function argument to the environment-based version seems to 
 * suggest that the environment should map identifiers to expression instead of values.
 *
 * However, we run into the same problems that we had with first-class functions before we introduced closures:
 * What happens to the deferred substitutions that still have to be applied in the function argument? If
 * we discard the environment in which the function argument was defined we again introduce a variant of
 * dynamic scoping.
 *
 * Hence, like for closures, we need to store the environment together with the expression. We call such a 
 * pair a _thunk_. An environment hence becomes a mapping from symbols to thunks. Note that environments
 * and thunks are hence mutually recursive. In Scala, we can hence not use type definitions of the form
 *
 * type Thunk = (Exp, Env)
 * type Env = Map[Symbol, Thunk]
 *
 * Instead, we use a Scala class Env to express this recursion.
 *
 * Since we want to experiment with different variants of how to generate and evaluate thunks we first
 * create a parameterizable variant of the evaluator that leaves open how to 
 * i)represent thunks (type Thunk)
 * ii) create thunks (method delay)
 * iii) evaluate thunks (method force).
 *
 * Hint: Research on the internet what abstract type members in Scala are.
 *       For instance, here: http://www.scala-lang.org/node/105
 */
trait CBN {
    type Thunk
    
    case class Env(map: Map[Symbol, Thunk]) {
      def apply(key: Symbol) = map.apply(key)
      def +(other: (Symbol, Thunk)) : Env = Env(map+other)
    }

    def delay(e: Exp, env: Env) : Thunk
    def force(t: Thunk) : Value

    // since values also depend on Env and hence on Thunk they need to
    // be defined within this trait    
    sealed abstract class Value
    case class NumV(n: Int) extends Value
    case class ClosureV(f: Fun, env: Env) extends Value 
    def eval(e: Exp, env: Env) : Value = e match {
      case Id(x) => force(env(x)) // force evaluation of thunk if identifier is evaluated
      case Add(l,r) => {
        (eval(l,env), eval(r,env)) match {
          case (NumV(v1),NumV(v2)) => NumV(v1+v2)
          case _ => sys.error("can only add numbers")
        }
      }
      case App(f,a) => eval(f,env) match {
        // delay argument expression and add it to environment of the closure
        case ClosureV(f,cenv) => eval(f.body, cenv + (f.param -> delay(a,env)))
        case _ => sys.error("can only apply functions")
      }
      case Num(n) => NumV(n)
      case f@Fun(x,body) => ClosureV(f,env)
    }
}
/* Let's now create an instance of CBN that corresponds to the substitution-based call-by-name
 * interpreter. A thunk is just a pair of expression and environment. Forcing a thunk just
 * evaluates it in the stored environment.
 *
 * To understand what is going on during evaluation of tests we trace argument evaluation by a 
 * printout to the console.
 */
 
object CallByName extends CBN {
  type Thunk = (Exp,Env)
  def delay(e: Exp, env: Env) = (e,env)
  def force(t: Thunk) = {
    println("Forcing evaluation of expression: "+t._1)
    eval(t._1,t._2)
  }
}

assert(CallByName.eval(test, CallByName.Env(Map.empty)) == CallByName.NumV(12))
/* Call-by-need
 * ------------
 * Call-by-name is rather wasteful: If an argument is used n times in the body, the argument
 * expression is re-evaluated n-times. For instance, in */
val cbntest = wth('double, Fun('x, Add('x,'x)),
               App('double, Add(2,3)))
/* the sum of 2 and 3 is computed twice. 
 * If the argument is passed again to another function,
 * this may lead to an exponential blow-up.
 *
 * Example: */

val blowup  = wth('a, Fun('x, Add('x,'x)),
              wth('b, Fun('x, Add(App('a,'x), App('a,'x))),
              wth('c, Fun('x, Add(App('b,'x), App('b,'x))),
              wth('d, Fun('x, Add(App('c,'x), App('c,'x))),
              wth('e, Fun('x, Add(App('d,'x), App('d,'x))),
              wth('f, Fun('x, Add(App('e,'x), App('e,'x))),
              App('f, Add(2,3))))))))
/*
 * Can we do better? Yes, by caching the value
 * when the argument expression is evaluated for the first time. This evaluation strategy
 * is called _call-by-need_. 
 *
 * Caching is easy to implement in Scala:
 */

 object CallByNeed extends CBN {
  case class MemoThunk(e: Exp, env: Env) {
    var cache: Value = null
  }
  type Thunk = MemoThunk
  def delay(e: Exp, env: Env) = MemoThunk(e,env)
  def force(t: Thunk) = {
    if (t.cache == null) {
      println("Forcing evaluation of expression: "+t.e)
      t.cache = eval(t.e, t.env)
    } else println ("Reusing cached value "+t.cache+" for expression "+t.e)
    t.cache
  }
}

/* For instance, compare call-by-need and call-by-name in cbntest or blowup. 
 *
 * However, the meta-language (i.e., the subset of Scala features) used in the
 * interpreter has become more complicated: Since we are using mutation, the order
 * of evaluation and aliasing of object references becomes important. Luckily, 
 * call-by-need agrees with call-by-name with regard to produced values and termination
 * behavior, hence it is usually not necessary to reason about programs with the
 * call-by-need semantics. If, however, one wants to reason about the performance
 * of a program in a call-by-need setting, one has to take these additional complications
 * into account. In practice, it is even worse, since languages like Haskell perform
 * additional optimizations that, for instance, switch to call-by-value if an analysis
 * can determine that an argument will definitely be used (lookup "strictness analysis"). 
 
 Topics for class discussion:
 - Is it a good idea to mix a language with implicit mutation (such as Java, Scala, C++, Python, ...)
   with lazy evaluation?
 - How can one simulate lazy evaluation in an eager language? 
   Basic idea: 'Lambda' as evaluation firewall.
 */
 

