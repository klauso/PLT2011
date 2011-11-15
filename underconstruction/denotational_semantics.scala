sealed abstract class Exp
case class Num(n: Int) extends Exp
case class Id(name: Symbol) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
implicit def num2exp(n: Int) = Num(n)
implicit def id2exp(s: Symbol) = Id(s)
case class Fun(param: Symbol, body: Exp) extends Exp
case class App (funExpr: Exp, argExpr: Exp) extends Exp
 
 // "with" would be a better name for this function, but it is reserved in Scala
def wth(x: Symbol, xdef: Exp, body: Exp) : Exp = App(Fun(x,body),xdef)

sealed abstract class Value
type Env = Map[Symbol, Value]
case class NumV(n: Int) extends Value
case class FunV(f: Value => Value) extends Value


def ds(e: Exp) : Env => Value = e match {
  case Num(n: Int) => (env) => NumV(n)
  case Id(x) => env => env(x)
  case Add(l,r) => { (env) =>
    (ds(l)(env), ds(r)(env)) match {
      case (NumV(v1),NumV(v2)) => NumV(v1+v2)
      case _ => sys.error("can only add numbers")
    }
  }
  case Fun(param,body) => (env) => FunV( (v) => ds(body)(env + (param -> v)))
  case App(f,a) => (env) => (ds(f)(env), ds(a)(env)) match {
    // Use environment stored in closure to realize proper lexical scoping!
    case (FunV(g),arg) => g(arg)
    case _ => sys.error("can only apply functions")
  }
}

val test2 = wth('x, 5, App(Fun('f, App('f,3)), Fun('y,Add('x,'y))))

case class Visitor[T](num: Int => T, add: (T,T)=>T, id: Symbol=>T, app: (T,T) => T, fun: (Symbol,T) => T )

def foldExp[T](v: Visitor[T], e: Exp) : T = {
  e match {
    case Num(n) => v.num(n)
    case Id(x) => v.id(x)
    case Add(l,r) => v.add(foldExp(v,l),foldExp(v,r))
    case App(l,r) => v.app(foldExp(v,l),foldExp(v,r))
    case Fun(x,body) => v.fun(x,foldExp(v,body))
  }
}

val dsvisitor = new Visitor[Env=>Value](
     n=>env=> NumV(n), 
    (l,r)=>env => (l(env), r(env)) match { case (NumV(v1),NumV(v2)) => NumV(v1+v2); case _ => sys.error("can only add numbers")  }, 
     x=>env=>env(x), 
    (f,a)=>env=> (f(env),a(env))  match { case (FunV(g),arg) => g(arg); case _ => sys.error("can only apply functions") },
    (x,body)=>env=>FunV( (v) => body(env+(x->v))))     