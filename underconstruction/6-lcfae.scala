sealed abstract class Exp
case class Num(n: Int) extends Exp
case class Id(name: Symbol) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
implicit def num2exp(n: Int) = Num(n)
implicit def id2exp(s: Symbol) = Id(s)
case class Fun(param: Symbol, body: Exp) extends Exp
case class App (funExpr: Exp, argExpr: Exp) extends Exp

def wth(x: Symbol, xdef: Exp, body: Exp) : Exp = App(Fun(x,body),xdef)
 
/* ===========================================
 * Substitution-based call-by-name interpreter
 * ===========================================
 */ 
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
}

def eval(e: Exp) : Exp = e match {
  case Id(v) => sys.error("unbound identifier: "+v)
  case Add(l,r) => (eval(l), eval(r)) match {
                     case (Num(x),Num(y)) => Num(x+y)
                     case _ => sys.error("can only add numbers")
                    }
  case App(f,a) => eval(f) match {
     case Fun(x,body) => eval( subst(body,x, a))
     case _ => sys.error("can only apply functions")
  }
  case _ => e 
}

val test = App( Fun('x,Add('x,5)), 7)
val test2 = wth('x, 5, App(Fun('f, App('f,3)), Fun('y,Add('x,'y))))

assert( eval(test) == Num(12))
assert(eval(test2) == Num(8))


/* ======================================================================
 * Environment-based call-by-name interpreter with strictness point in Id
 * ======================================================================
 */ 

// Is there a better way to define the recursive type Env1 = Map[Symbol,Env1]?
case class Env1(map: Map[Symbol, (Exp,Env1)]) {
  def apply(key: Symbol) = map.apply(key)
  def +(other: (Symbol, (Exp,Env1))) : Env1 = Env1(map+other)
}

sealed abstract class Value1
case class NumV1(n: Int) extends Value1
case class ClosureV1(f: Fun, env: Env1) extends Value1

def evalWithEnv1(e: Exp, env: Env1) : Value1 = e match {
  case Id(x) => env(x) match {
    case (e2,env2) => evalWithEnv1(e2,env2)
  }
  case Add(l,r) => {
    (evalWithEnv1(l,env), evalWithEnv1(r,env)) match {
      case (NumV1(v1),NumV1(v2)) => NumV1(v1+v2)
      case _ => sys.error("can only add numbers")
    }
  }
  case App(f,a) => evalWithEnv1(f,env) match {
    case ClosureV1(f,cenv) => evalWithEnv1(f.body, cenv + (f.param -> (a,env)))
    case _ => sys.error("can only apply functions")
  }
  case Num(n) => NumV1(n)
  case f@Fun(x,body) => ClosureV1(f,env)
}

assert( evalWithEnv1(test, Env1(Map.empty)) == NumV1(12))
assert(evalWithEnv1(test2, Env1(Map.empty)) == NumV1(8))

/* ================================================================================
 * Environment-based call-by-name interpreter with strictness points in Add and App
 * ================================================================================
 */ 

sealed abstract class Value2
type Env2 = Map[Symbol, Value2]
case class NumV2(n: Int) extends Value2
case class ClosureV2(f: Fun, env: Env2) extends Value2
case class ExprClosure(e: Exp, env: Env2) extends Value2

// putting it in an object to enable mutual recursion
object eval2 {
    def strict(v: Value2) : Value2 = v match {
      case ExprClosure(e,env) => strict(evalWithEnv2(e,env))
      case _ => v
    }
    def evalWithEnv2(e: Exp, env: Env2) : Value2 = e match {
      case Id(x) => env(x) 
      case Add(l,r) => {
        (strict(evalWithEnv2(l,env)), strict(evalWithEnv2(r,env))) match {
          case (NumV2(v1),NumV2(v2)) => NumV2(v1+v2)
          case _ => sys.error("can only add numbers")
        }
      }
      case App(f,a) => strict(evalWithEnv2(f,env)) match {
        case ClosureV2(f,cenv) => evalWithEnv2(f.body, cenv + (f.param -> ExprClosure(a,env)))
        case _ => sys.error("can only apply functions")
      }
      case Num(n) => NumV2(n)
      case f@Fun(x,body) => ClosureV2(f,env)
    }
}
def evalWithEnv2(e: Exp, env: Env2) : Value2 = eval2.strict(eval2.evalWithEnv2(e,env))

assert(evalWithEnv2(test, Map.empty) == NumV2(12))
assert(evalWithEnv2(test2,Map.empty) == NumV2(8))

// Question: Is evalWithEnv1 == evalWithEnv2 ?