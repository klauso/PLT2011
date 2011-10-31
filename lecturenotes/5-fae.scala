
sealed abstract class Exp
case class Num(n: Int) extends Exp
case class Id(name: Symbol) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
implicit def num2exp(n: Int) = Num(n)
implicit def id2exp(s: Symbol) = Id(s)

case class Fun(param: Symbol, body: Exp) extends Exp
case class App (funExpr: Exp, argExpr: Exp) extends Exp

def wth(x: Symbol, xdef: Exp, body: Exp) : Exp = App(Fun(x,body),xdef)


def freshName(names: Set[Symbol], default: Symbol) : Symbol = {
  if (! (names contains default)) return default
  var last : Int = 0  
  while (names contains Symbol(default.name+last.toString)) last = last+1
  Symbol(default.name+last.toString)
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
  case Fun(param,body) => if (param == x) e1 
                          else {
                              val newvar = freshName(freeVars(e2), param)
                              Fun(newvar, subst(subst(body, param, Id(newvar)), x, e2))
                          }                            
}

assert( subst(Add(5,'x), 'x, 7) == Add(5, 7))
assert( subst(Add(5,'x), 'y, 7) == Add(5,'x))
assert( subst(Fun('x, Add('x,'y)), 'x, 7) == Fun('x, Add('x,'y)))

// test capture-avoiding substitution
assert( subst(Fun('x, Add('x,'y)), 'y, Add('x,5)) == Fun('x0,Add(Id('x0),Add(Id('x),Num(5)))))

def eval(e: Exp) : Exp = e match {
  case Add(l,r) => (eval(l), eval(r)) match {
                     case (Num(x),Num(y)) => Num(x+y)
                     case _ => sys.error("can only add numbers")
                    }
  case Id(v) => sys.error("eval on id")
  case App(f,a) => eval(f) match {
     case Fun(x,body) => eval( subst(body,x, eval(a)))
     case _ => sys.error("can only apply functions")
  }
  case _ => e
}

val test = App( Fun('x,Add(Id('x),Num(5))), Num(7))

val test2 = App(App(Fun('z, Fun('y, App('z, 3)) ), Fun('x,'y)),4)
assert( eval(test) == Num(12))

sealed abstract class Value

type Env = Map[Symbol, Value]

case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value



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
    case ClosureV(f,closureEnv) => evalWithEnv(f.body, closureEnv + (f.param -> evalWithEnv(a,env)))
    case _ => sys.error("can only apply functions")
  }
}
assert( evalWithEnv(test, Map.empty) == NumV(12))