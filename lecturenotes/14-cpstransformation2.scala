/*
These are lecture notes for the "Programming Languages and Types" course by Klaus Ostermann 
at the University of Marburg

loosely based on Sec. 18 of "Programming Languages: Application and Interpretation" by
Shriram Krishnamurthi

Please comment/correct/improve these notes via github. Proposals or questions can
be submitted as an "issue"; proposals for corrections/extensions/improvements can
be submitted as a "pull request". You can of course also send an email to Klaus Ostermann */


/* Today's goal is to make the "web" (or rather, CPS) transformation which we applied informally
  in the previous lecture formal.
  
  In the previous lecture we have seen that we had to translate the following program:

  println("The sum is: "+ (inputNumber("First number:" ) + inputNumber("Second number")))

  into this program:

  webread_k("First number", (n) =>
             webread_k("Second number:", (m) => webdisplay("The sum is: "+(n+m))))
             
  This hand-translation is sufficient if this expression is the entire program.
  
  If we wish to use it as a sub-expression in a larger program, this does not suffice,
  because there may be a pending computation outside its own evaluation. For that
  reason, the program has to send its result to a continuation instead of returning it:

  k => webread_k("First number", (n) =>
             webread_k("Second number:", (m) => webdisplay("The sum is: "+k(n+m))))
             
  This version can be employed in the transformation of a larger program. In the special
  case where this is the entire program we can apply the transformed term to the identity
  function to get the same result as the previous manual transformation.

  In general, every term, when converted to CPS, will be have the following properties
  (see O. Danvy, Three Steps for the CPS Transformation, 1991):
  
  1) The values of all intermediate applications are given a name
  2) The evaluation of these applications is sequentialized based on a traversal of their
     abstract syntax tree.
  3) The results of the transformation are procedures that expect a continuation parameter -
     a lambda abstraction whose application to intermediate values yields the final result 
     of the whole evaluation.
     
  Let us now look at the transformation of a function application. For instance, let us 
  consider the term 
    f(a)(g(b))  
  The transformation of the function argument, f(a), should be
    f_k(a, fval => ...)
  Similarly, the transformation of the argument position would be:
    g_k(b, aval => ...)
  Given these two values, fval and aval, we can now perform the application, like so:
    k(fval(aval))
  However, this will not work, because if fval makes a web interaction itself it will not return.
  Instead, k must be given as an argument to the function, like so:
    k => f_k(a, fval => g_k(b, aval => fval(aval,k))) 
  Reading this sequentially, it says to evaluate the function expression, store its value in fval,
  then evaluate the argument, store its value in aval, and finally invoke the function on the argument.
  This function's continuation is the same as that of the function application itself.
  
  What about variables and constants? Since every term in CPS must be a function that consumes a continuation,
  the constant is simply send to the continuation. For instance, the CPS transformation of 
  the number 3 is k => k(3)
  
  What about function definitions, such as x => x ? Since every lambda expression is also a constant,
  we might be tempted to use the same rule as above, i.e.,
    k => k(x => x)
  However, the transformation is more subtle. A function application invokes the function on two arguments, whereas
  the original function x=>x consumes only one. What is the second argument?
  
  Answer: It is the _dynamic_ continuation, i.e., the continuation at the time of the function _application_
  (as opposed to its definition). We cannot ignore this continuation: It is the stack active at the point
  of function invocation, so we want to preserve it. This is in contrast to what we did with environments,
  and more in line with our treatment of the store. The transformed version hence reads:
  
  k => k( (x,dynk) => (k => k(x))(dynk))

  which is equivalent (when the inner application finally happens) to:
  
  k => k( (x,dynk) => dynk(x))
  
  This is a function that accepts a value and a dynamic continuation and sends the value to that continuation.
  
  We are now reade to write this transformation formally, as a source-to-source transformation. This transformation
  could have the type cps(e: Exp): Exp, but we choose a different type for two reasons:
  1) In CPS we need function definitions and applications with two arguments instead of one. This could be addressed
     by adding new syntax.
  2) More importantly, we want the invariants of the CPS format to be clearly visible in the syntax definition of the
     result, most importantly the fact that all function applications are tail calls.
  For this reason, we define a special new syntax for CPS-transformed terms. Here is our original syntax: */     
  
sealed abstract class Exp
case class Num(n: Int) extends Exp
case class Id(name: Symbol) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Fun(param: Symbol, body: Exp) extends Exp
case class App (funExpr: Exp, argExpr: Exp) extends Exp
implicit def num2exp(n: Int) = Num(n)
implicit def id2exp(s: Symbol) = Id(s)

/* For CPS transformed terms, we define two different syntactic categories: Values (CPSVal) and Expressions (CPSExp).
   The syntax makes clear that all arguments of a function application are values - hence no nesting of applications
   can occur. Furthermore, the syntax differentiates between defining an ordinary function (CPSFun) which, when translated,
   gets an additional continuation parameter, and Continuation Functions (CPSCont), which are the result of the CPS 
   transformation. Correspondingly, we have two different forms of applications, CPSContApp and CPSFunApp.
   
   Here is the formal definition: */
sealed abstract class CPSVal
abstract class CPSExp extends CPSVal
case class CPSNum(n: Int) extends CPSVal
case class CPSCont(v: Symbol, body: CPSExp) extends CPSVal
case class CPSFun(x: Symbol, k: Symbol, body: CPSExp) extends CPSVal
case class CPSVar(x: Symbol) extends CPSVal { override def toString = x.toString }
implicit def id2cpsexp(x: Symbol) = CPSVar(x)

case class CPSContApp(k: CPSVal, a: CPSVal) extends CPSExp
case class CPSFunApp(f: CPSVar, a: CPSVar, k: CPSVar) extends CPSExp // the arguments are even CPSVar and not only CPSVal!
case class CPSAdd(l: CPSVar, r: CPSVar) extends CPSExp

/* With these definitions, we are now ready to formalize the transformation described above.
 * There is one technical issues: We need to introduce new names for binders into our program, such as 'k.
 * We need to make sure that we do not accidentially capture existing names in the program. For this
 * reason we need our freshName machinery we introduced in 5-fae.scala.
 */
def freeVars(e: Exp) : Set[Symbol] =  e match {
   case Id(x) => Set(x)
   case Add(l,r) => freeVars(l) ++ freeVars(r)
   case Fun(x,body) => freeVars(body) - x
   case App(f,a) => freeVars(f) ++ freeVars(a)
   case Num(n) => Set.empty
}
def freshName(names: Set[Symbol], default: Symbol) : Symbol = {
  var last : Int = 0
  var freshName = default  
  while (names contains freshName) { freshName = Symbol(default.name+last.toString); last += 1; }
  freshName
}

def cps(e: Exp) : CPSCont = e match {
   case Add(e1,e2) => {
     val k = freshName(freeVars(e), 'k)
     val lv = freshName(freeVars(e2), 'lv)
     CPSCont(k, CPSContApp(cps(e1),CPSCont(lv, CPSContApp(cps(e2), CPSCont('rv, CPSContApp(k,CPSAdd('rv, lv)))))))
   }  
   case Fun(a, body) => {
     val k = freshName(freeVars(e), 'k)
     val dynk = freshName(freeVars(e), 'dynk)
     CPSCont(k, CPSContApp(k, CPSFun(a, dynk, CPSContApp(cps(body), dynk))))
   }
   case App(f,a) => {
     val k = freshName(freeVars(e), 'k)
     val fval = freshName(freeVars(a), 'fval)
     CPSCont(k, CPSContApp(cps(f), CPSCont(fval, CPSContApp(cps(a), CPSCont('aval, CPSFunApp(fval, 'aval, k))))))
   }
   case Id(x) => {
     val k = freshName(freeVars(e), 'k)
     CPSCont(k, CPSContApp(k, CPSVar(x)))
   }
   case Num(n) => {
     val k = freshName(freeVars(e), 'k)
     CPSCont('k, CPSContApp('k,CPSNum(n)))
   }
}

/* This transformation is the so-called Fischer CPS transformation. There are many other CPS transformation algorithms.
   The Fischer CPS transformation is nice because it is so simple and because it is defined as one simple structural 
   recursion over the AST. Its main disadvantage is the existence of so-called "administrative redexes". 
   An administrative redex is a function application whose operator is a "continuation lambda" - a lambda produced during
   CPS transformation that was not in the original program. Such function applications can be computed immediately because 
   the function which is called is known.
   
   For instance, cps(Add(2,3)) yields 
   
   CPSCont('k,
           CPSContApp(
             CPSCont('k,
                     CPSContApp('k,2)),
             CPSCont('lv,
                     CPSContApp(
                       CPSCont('k,
                               CPSContApp('k,3)),
                       CPSCont('rv,
                               CPSAdd('rv,'lv))))))
                               
    instead of 
    
    CPSCont('k, CPSContApp('k, CPSAdd(2,3)))
    
    Many more advanced CPS transformation algorithms try to avoid as many administrative redexes as possible.
*/