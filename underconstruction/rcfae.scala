sealed abstract class Exp
case class Num(n: Int) extends Exp
case class Id(name: Symbol) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class If0(cond: Exp, thenExp: Exp, elseExp: Exp) extends Exp
case class Letrec(x: Symbol, e: Exp, body: Exp) extends Exp
implicit def num2exp(n: Int) = Num(n)
implicit def id2exp(s: Symbol) = Id(s)
case class Fun(param: Symbol, body: Exp) extends Exp
case class App (funExpr: Exp, argExpr: Exp) extends Exp
def wth(x: Symbol, xdef: Exp, body: Exp) : Exp = App(Fun(x,body),xdef)

sealed abstract class Value

class BoxedValue(var x: Value)

type Env = Map[Symbol, Either[Value,BoxedValue]]

def lookupEnv(x: Symbol, env: Env): Value = env(x) match {
  case Left(v) => v
  case Right(b) => b.x
}

case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value

val sum = Letrec('sum, Fun('n, If0('n, 0, Add('n, App('sum, Add('n,-1))))), App('sum, 10))

def eval(e: Exp, env: Env) : Value = e match {
  case Num(n: Int) => NumV(n)
  case Id(x) => lookupEnv(x,env)
  case If0(cond, thenExp, elseExp) => eval(cond,env) match {
    case NumV(0) => eval(thenExp,env)
    case _ => eval(elseExp,env)
  }    
  case Letrec(x,e,body) => {
    val b = new BoxedValue(null)
    val cyclicenv : Env =  env + (x -> Right(b))
    b.x = eval(e,cyclicenv)
    eval(body,cyclicenv)
  }
  case Add(l,r) => {
    (eval(l,env), eval(r,env)) match {
      case (NumV(v1),NumV(v2)) => NumV(v1+v2)
      case _ => sys.error("can only add numbers")
    }
  }
  case f@Fun(param,body) => ClosureV(f, env)
  case App(f,a) => eval(f,env) match {
    case ClosureV(f,closureEnv) => eval(f.body, closureEnv + (f.param -> Left(eval(a,env))))
    case _ => sys.error("can only apply functions")
  }
}
