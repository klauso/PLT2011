/*
Stephan Wöllauer
Florian Heß
Simon Fett
*/

/*
Interpreter for recursive language
==================================

In the lecture you have seen an environment-based interpreter for RCFAE. Write a
substitution-based interpreter for RCFAE. Discuss the differences of these
interpreters with respect to recursion. Would have implementing a dynamically
scoped recursive language been easier?
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

case class Letrec(x: Symbol, e: Exp, body: Exp) extends Exp

def freshName(names: Set[Symbol], default: Symbol) : Symbol = {
  var last : Int = 0
  var freshName = default  
  while (names contains freshName) { freshName = Symbol(default.name+last.toString); last += 1; }
  freshName
}

def freeVars(e: Exp) : Set[Symbol] =  e match {
   case Id(x) => Set(x)
   case Add(l,r) => freeVars(l) ++ freeVars(r)
   case Fun(x,body) => freeVars(body) - x
   case App(f,a) => freeVars(f) ++ freeVars(a)
   case Num(n) => Set.empty
   case If0(cond, exp1, exp2) => freeVars(cond) ++ freeVars(exp1) ++ freeVars(exp2)
   case Letrec(x, exp1, body) => (freeVars(exp1) ++ freeVars(body)) - x
}

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
  case If0(cond, exp1, exp2) => If0(subst(cond,x,e2), subst(exp1,x,e2), subst(exp2,x,e2))
  case Letrec(param, exp1, body) => if(param == x) e1 else {
		val newvar = freshName(freeVars(e2), param)
		Letrec(newvar, subst(subst(exp1, param, Id(newvar)), x, e2), subst(subst(body, param, Id(newvar)), x, e2))
  }
}

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
  case Num(n) => e
  case Fun(param,body) => e
  case If0(cond, exp1, exp2) => eval(cond) match {
	  case Num(n) => if(n==0) eval(exp1) else eval(exp2)
	  case _ => sys.error("if-condition needs to be a number")
  }
  case Letrec(x, exp1, body) =>
    eval(subst(body, x, subst(exp1, x, Letrec(x, exp1, x))))
}


val sum = Letrec('sum, Fun('n, If0('n, 0, Add('n, App('sum, Add('n,-1))))), App('sum, 10))

assert(eval(sum) == Num(55))

