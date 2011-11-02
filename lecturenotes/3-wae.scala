/*
These are lecture notes for the "Programming Languages and Types" at the University of Marburg

loosely based on Sec. 3 of the book 
"Programming Languages: Application and Interpretation" by Shriram Krishnamurthi

Please send comments or errors in these notes via email to Klaus Ostermann.
My email address can be found on my webpage. Alternatively, you can also 
propose corrections as a github pull request.

*/

/*
 * We want, step by step, to develop our primitive calculator language into
 * a full-fledged PL. 
 
 * One important milestone on this way is the ability to deal with names.
 * While our previous language allowed expressions with identifiers in them,
 * it had no _binders_ : Constructs that allow to give meaning to a new name.
 
 * In this variant of the language, called WAE, we introduce such a binder
 * called "with" with which we can give an expression a name that can be used
 * in the body of the "with" expression. This intuition is captured in the 
 * definition of the "With" case class below, which extends our previous 
 * language. 
 
 * We study this WAE language to better understand what names mean in 
 * programming languages, and how they can be implemented.
 
 */

sealed abstract class Exp 
case class Num(n: Int) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Mul(lhs: Exp, rhs: Exp) extends Exp
case class Id(x: Symbol) extends Exp 
case class With(x: Symbol, xdef: Exp, body: Exp) extends Exp
 
/* We use implicits again to make example programs less verbose. */
implicit def num2exp(n: Int) = Num(n)
implicit def sym2exp(x: Symbol) = Id(x)

/* A first example program in WAE. */
val test = With('x, 5, Add('x,'x))

/* Note that we deal with *two* languages here:
   
   (1) This a Scala file with Scala code.
   (2) Most of the functions work on programs written in the WAE language.
   
   Most of the time, we concentrate on WAE, but sometimes, we also
   talk about Scala.
   
   We have not defined a concrete syntax for WAE, but it is a
   real language nevertheless. We sometimes use some made-up
   syntax for examples on the blackboard or in comments.
 */
   
/* 
 * Instead of dealing with identifiers as external entities as in AE, 
 * identifiers can now be defined within the language. This justifies
 * a new treatment of identifiers. We will explain them in terms of
 * _substitution_, a notion well-known informally from Gymnasium algebra.
 
 * The idea is the following: The interpreter transforms the term
 *
 *   with (x = 5) {
 *     x + x
 *   }
 * 
 * into
 *   
 *   5 + 5
 * 
 * before proceeding. That is, all occurrences of x have been
 * replaced by 5.
 
 * Note that these two programs -- before and after the substitution --
 * are certainly not *equal*: They look quite different. However,
 * they are *equivalent* in the sense that when evaluated, they
 * will produce the same number. Such transformations between 
 * different but somehow equivalent programs are an important
 * tool for the study of programs, and of programming languages.
 * Often, if we know which programs behave identically, we understand
 * better how programs behave in general. We will see more examples
 * of this in this lecture.
 
 * Hence, the implementation of the "With" case of our interpreter should
 * be something like:
 * 
 *     case With(x, xdef, body) => eval(subst(body,x,Num(eval(xdef))))
 * 
 * for a function subst with signature
 * 
 *     subst: (Exp,Symbol,Num)=>Exp)
 *
 * The type of the third parameter is "Num" instead of "Exp" because it 
 * is more difficult to get substitution correct when arbitrary expressions
 * can be inserted (accidential name capture problem, more about that later).
 *
 * Since we want to experiment with different versions of substitution, we
 * write the interpreter in such a way that we can parameterize it with
 * a substitution function:
 */
def makeEval(subst: (Exp,Symbol,Num)=>Exp) : Exp=>Int = {
  def eval(e: Exp) : Int = e match {
    case Num(n) => n
    case Id(x) => sys.error("unbound variable: "+x)
    case Add(l,r) => eval(l) + eval(r)
    case Mul(l,r) => eval(l) * eval(r)
    case With(x, xdef, body) => eval(subst(body,x,Num(eval(xdef)))) // take the int and wrap it into a Num
  }
  eval
}

/* Substitution, take 1:
 * To substitute identiﬁer i in e with expression v, replace all identiﬁers in e that
 * have the name i with the expression v 
 
 * Let's try to formalize this definition: */
 
 /*
    
 val subst1 : (Exp,Symbol,Num) => Exp = (e,i,v) => e match {
    case Num(n) => e
    case Id(x) => if (x == i) v else e
    case Add(l,r) => Add( subst1(l,i,v), subst1(r,i,v))
    case Mul(l,r) => Mul( subst1(l,i,v), subst1(r,i,v))
    case With(x,xdef,body) => With( if (x ==i) v else x,
                                    subst1(xdef,i,v),
                                    subst1(body,i,v))
}
*/

/* 
 * Unfortunately this does not even type-check! And rightly so, because 
 * it might otherwise turn reasonable programs into programs that are
 * not even syntactically legal anymore.
 * 
 * Exercise for self-study: Find an expression that would be transformed
 * into one that is not syntactically legal.

 * To get substitution correct, we need to define some terminology:
 
 * Deﬁnition(Binding Instance): A binding instance of an identiﬁer is the instance of the identiﬁer that
 * gives it its value. In WAE , the 'x' position of a with is the only binding instance.

 * Deﬁnition (Scope) The scope of a binding instance is the region of program text in which instances of the
 * identiﬁer refer to the value bound by the binding instance.

 * Deﬁnition (Bound Instance) An identiﬁer is bound if it is contained within the scope of a binding in-
 * stance of its name.

 * Deﬁnition (Free Instance) An identiﬁer not contained in the scope of any binding instance of its name is
 * said to be free.
 
 * Examples: In WAE, the symbol in Id('x) is a bound or free instance, and the
 * symbol in With('x, ..., ...) is a binding instance. The scope of this
 * binding instance is the third sub-term of With.
 
 * OK, with this new terminology, we can make another take at substitution:
 
 * Deﬁnition (Substitution, take 2) To substitute identiﬁer i in e with expression v, replace all identiﬁers in
 * e which are not binding instances that have the name i with the expression v.
 
 * Here is the formalization of this definition.
 */
val subst2 : (Exp,Symbol,Num) => Exp = (e,i,v) => e match {
    case Num(n) => e
    
    // Bound or free instance => substitute if names match
    case Id(x) => if (x == i) v else e
    
    case Add(l,r) => Add( subst2(l,i,v), subst2(r,i,v))
    case Mul(l,r) => Mul( subst2(l,i,v), subst2(r,i,v))
    
    // binding instance => do not substitute
    case With(x,xdef,body) => With( x,
                                    subst2(xdef,i,v),
                                    subst2(body,i,v))
}

// Let's create an interpreter that uses this substitution function.
def eval2 = makeEval(subst2)

assert(eval2(test) == 10) // it works!

val test2 = With('x, 5, Add('x, With('x, 3,10))) // another test

assert(eval2(test2) == 15) // works as expected

val test3 = With('x, 5, Add('x, With('x, 3,'x))) // another test

// assert(eval2(test3) == 8) // Bang! Result is 10 instead!

/* 
 * What went wrong here? Our substitution algorithm respected binding instances, but not their scope.
 * In the sample expression, the with introduces a new scope for the inner x . The scope of the outer x
 * is shadowed or masked by the inner binding. Because substitution doesn’t recognize this possibility, it
 * incorrectly substitutes the inner x . 
 
 * Deﬁnition (Substitution, take 3) To substitute identiﬁer i in e with expression v, replace all non-binding
 * identiﬁers in e having the name i with the expression v, unless the identiﬁer is in a scope different from that
 * introduced by i */
 
 val subst3 : (Exp,Symbol,Num) => Exp = (e,i,v) => e match {
    case Num(n) => e
    case Id(x) => if (x == i) v else e
    case Add(l,r) => Add( subst3(l,i,v), subst3(r,i,v))
    case Mul(l,r) => Mul( subst3(l,i,v), subst3(r,i,v))
    case With(x,xdef,body) => With( x,
                                    subst3(xdef,i,v),
                                    // do not substitute in nested scopes
                                    body)
}

def eval3 = makeEval(subst3)

assert(eval3(test) == 10)

assert(eval3(test2) == 15)

val test3 = With('x, 5, Add('x, With('x, 3,'x)))

assert(eval3(test3) == 8) // Success!

val test4 = With('x, 5, Add('x, With('y, 3,'x)))

// assert(eval3(test4) == 10) // Bang! unbound variable: 'x

/* 
 * The inner expression should result in an error, because x has no value. Once again, substitution has changed
 * a correct program into an incorrect one!
 * Let’s understand what went wrong. Why didn’t we substitute the inner x ? Substitution halts at the with
 * because, by deﬁnition, every with introduces a new scope, which we said should delimit substitution. But
 * this with contains an instance of x, which we very much want substituted! So which is it — substitute
 * within nested scopes or not? Actually, the two examples above should reveal that our latest deﬁnition for
 * substitution, which may have seemed sensible at ﬁrst blush, is too draconian: it rules out substitution within
 * any nested scopes.

 * Deﬁnition (Substitution, take 4) To substitute identiﬁer i in e with expression v, replace all non-binding
 * identiﬁers in e having the name i with the expression v, except within nested scopes of i.
 
 * Finally, we have a version of substitution that works. A different, more succint way of phrasing this
 * deﬁnition is

 * Deﬁnition Substitution, take 5) To substitute identiﬁer i in e with expression v, replace all free instances
 * of i in e with v.
 */
 
 val subst4 : (Exp,Symbol,Num) => Exp = (e,i,v) => e match {
    case Num(n) => e
    case Id(x) => if (x == i) v else e
    case Add(l,r) => Add( subst4(l,i,v), subst4(r,i,v))
    case Mul(l,r) => Mul( subst4(l,i,v), subst4(r,i,v))
    // do not substitute when shadowed
    case With(x,xdef,body) => if (x == i) e 
                                 else With(x,
                                           subst4(xdef,i,v),
                                           subst4(body,i,v))
}
 
def eval4 = makeEval(subst4)

assert(eval4(test) == 10)

assert(eval4(test2) == 15)

assert(eval4(test3) == 8) 

assert(eval4(test4) == 10) // Success!

val test5 = With('x, 5, With('x, 'x, 'x))

// assert(eval4(test5) == 5) // Bang! unbound variable 'x

/*
 * This program should evaluate to 5, but it too halts with an error. This is because we prematurely stopped
 * substituting for x. We should substitute in the named expression of a with even if the with in question
 * deﬁnes a new scope for the identiﬁer being substituted, because its named expression is still in the scope of
 * the enclosing binding of the identiﬁer.
 * We ﬁnally get a valid programmatic deﬁnition of substitution (relative to the language we have so far):
 */
 
 val subst5 : (Exp,Symbol,Num) => Exp = (e,i,v) => e match {
    case Num(n) => e
    case Id(x) => if (x == i) v else e
    case Add(l,r) => Add( subst5(l,i,v), subst5(r,i,v))
    case Mul(l,r) => Mul( subst5(l,i,v), subst5(r,i,v))
    // handle shadowing correctly
    case With(x,xdef,body) => With(x,
                                   subst5(xdef,i,v),
                                   if (x == i) body else subst5(body,i,v))
}
 
def eval5 = makeEval(subst5)

assert(eval5(test) == 10)

assert(eval5(test2) == 15)

assert(eval5(test3) == 8) 

assert(eval5(test4) == 10) 

assert(eval5(test5) == 5) // Success!

 
 /* SUMMARY:
  *
  *  (1) Substitution can be used to understand the meaning of
  *      names in programming languages.
  *
  *  (2) Correct implementations of substitution need to handle
  *      free, bound, and binding instances of names and their
  *      scopes correctly.
  */
     