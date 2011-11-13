sealed abstract class Exp
case class Num(n: Int) extends Exp
case class Id(name: Symbol) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class If0(cond: Exp, thenExp: Exp, elseExp: Exp) extends Exp
case class NewBox(e: Exp) extends Exp
case class SetBox(b: Exp, e: Exp) extends Exp
case class Seq(e1: Exp, e2: Exp) extends Exp
implicit def num2exp(n: Int) = Num(n)
implicit def id2exp(s: Symbol) = Id(s)
case class Fun(param: Symbol, body: Exp) extends Exp
case class App (funExpr: Exp, argExpr: Exp) extends Exp
def wth(x: Symbol, xdef: Exp, body: Exp) : Exp = App(Fun(x,body),xdef)

type Address = Int
var _nextAddress : Address = 0
def nextAddress : Address = {
  _nextAddress += 1
  _nextAddress
}
  
sealed abstract class Value

type Env = Map[Symbol, Value]

case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value
case class AddressV(a: Int) extends Value

type Store = Map[Address, Value]

def eval(e: Exp, env: Env, s: Store) : (Value, Store) = e match {
  case Num(n: Int) => (NumV(n),s)
  case Id(x) => (env(x),s)
  case If0(cond, thenExp, elseExp) => eval(cond,env,s) match {
    case (NumV(0),s2) => eval(thenExp,env,s2)
    case (_,s2) => eval(elseExp,env,s2)
  }    
  case Add(l,r) => {
    eval(l,env,s) match {
      case (NumV(v1),s2) => eval(r,env,s2) match {
        case (NumV(v2),s3) => (NumV(v1+v2),s3)
        case _ => sys.error("can only add numbers")
      }
      case _ => sys.error("can only add numbers")
    }
  }
  case f@Fun(param,body) => (ClosureV(f, env),s)
  case App(f,a) => eval(f,env,s) match {
    case (ClosureV(f,closureEnv),s2) => eval(a,env,s2) match {
      case (av,s3) => eval(f.body, closureEnv + (f.param -> av), s3)
    }
    case _ => sys.error("can only apply functions")
  }
  case Seq(e1,e2) => eval(e2,env,eval(e1,env,s)._2) 
  case NewBox(e: Exp) => eval(e,env,s) match {
    case (v,s2) => {
       val a = nextAddress
       (AddressV(a), s2 + (a -> v))
    }
  }
  case SetBox(b: Exp, e: Exp) => eval(b,env,s) match {
    case (AddressV(a),s2) => eval(e,env,s2) match {
       case (ev, s3) => (ev, s3.updated(a,ev))
    }
    case _ => sys.error("can only set boxes")
  }
}
