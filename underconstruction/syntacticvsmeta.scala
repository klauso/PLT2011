object HOAS {
    sealed abstract class Exp
    case class Num(n: Int) extends Exp
    case class Id(name: Symbol) extends Exp
    case class Add(lhs: Exp, rhs: Exp) extends Exp
    case class Fun(f: Exp => Exp) extends Exp
    case class App (funExpr: Exp, argExpr: Exp) extends Exp
    def eval(e: Exp) : Exp = e match {
      case Id(v) => sys.error("unbound identifier: "+v)
      case Add(l,r) => (eval(l), eval(r)) match {
                         case (Num(x),Num(y)) => Num(x+y)
                         case _ => sys.error("can only add numbers")
                        }
      case App(f,a) => eval(f) match {
         case Fun(f) => eval( f(eval(a)))
         case _ => sys.error("can only apply functions")
      }
      case _ => e // numbers and functions evaluate to themselves
    }      
}

sealed abstract class Exp
case class Num(n: Int) extends Exp
case class Id(name: Symbol) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Fun(param: Symbol, body: Exp) extends Exp
case class App (funExpr: Exp, argExpr: Exp) extends Exp

object Compositional { 
    sealed abstract class Value
    type Env = Map[Symbol, Value]
    case class NumV(n: Int) extends Value
    case class FunV(f: Value => Value) extends Value

    def eval(e: Exp) : Env => Value = e match {
      case Num(n: Int) => (env) => NumV(n)
      case Id(x) => env => env(x)
      case Add(l,r) => { (env) =>
        (eval(l)(env),  eval(r)(env)) match {
          case (NumV(v1),NumV(v2)) => NumV(v1+v2)
          case _ => sys.error("can only add numbers")
        }
      }
      case Fun(param,body) => (env) => FunV( (v) => eval(body)(env + (param -> v)))
      case App(f,a) => (env) => (eval(f)(env), eval(a)(env)) match {
        // Use environment stored in closure to realize proper lexical scoping!
        case (FunV(g),arg) => g(arg)
        case _ => sys.error("can only apply functions")
      }
    }
}

object FAE {
    sealed abstract class Value
    type Env = Map[Symbol, Value]
    case class NumV(n: Int) extends Value
    case class ClosureV(f: Fun, env: Env) extends Value

    def eval(e: Exp, env: Env) : Value = e match {
      case Num(n: Int) => NumV(n)
      case Id(x) => env(x)
      case Add(l,r) => {
        (eval(l,env), eval(r,env)) match {
          case (NumV(v1),NumV(v2)) => NumV(v1+v2)
          case _ => sys.error("can only add numbers")
        }
      }
      case f@Fun(param,body) => ClosureV(f, env)
      case App(f,a) => eval(f,env) match {
        // Use environment stored in closure to realize proper lexical scoping!
        case ClosureV(f,closureEnv) => eval(f.body, closureEnv + (f.param -> eval(a,env)))
        case _ => sys.error("can only apply functions")
      }
    }
}

object CPSTransformed {
    sealed abstract class Value
    type Env = Map[Symbol, Value]
    case class NumV(n: Int) extends Value
    case class ClosureV(f: Fun, env: Env) extends Value
     
    def eval[T](e: Exp, env: Env, k: Value =>T) : T = e match {
      case Num(n: Int) => k(NumV(n))
      case Id(x) => k(env(x))
      case Add(l,r) => {
        eval(l,env, lv => 
            eval(r,env, rv =>
              (lv,rv) match {
                case (NumV(v1), NumV(v2)) => k(NumV(v1+v2))
                case _ => sys.error("can only add numbers")
              }))
      }
      case f@Fun(param,body) => k(ClosureV(f, env))
      case App(f,a) => eval(f,env, cl => cl match {
                case ClosureV(f,closureEnv) => eval(a,env, av => eval(f.body, closureEnv + (f.param -> av),k))
                case _ => sys.error("can only apply functions")
      })
    }
}


object LambdaLifted {
    sealed abstract class Value
    type Env = Map[Symbol, Value]
    case class NumV(n: Int) extends Value
    case class ClosureV(f: Fun, env: Env) extends Value
    def addc1[T](r: Exp, env: Env, k: Value => T)(lv: Value) =  
            eval(r,env, addc2(lv,k))
    def addc2[T](lv: Value, k: Value => T)(rv: Value) = 
               (lv,rv) match {
                case (NumV(v1), NumV(v2)) => k(NumV(v1+v2))
                case _ => sys.error("can only add numbers") 
              }
    def evalc1[T](a: Exp, env: Env, k: Value => T)(cl: Value) = cl match {
                case ClosureV(f,closureEnv) => eval(a,env, evalc2(f, closureEnv, k))
                case _ => sys.error("can only apply functions")
    }
    def evalc2[T](f: Fun, closureEnv : Env, k: Value => T)(av: Value) = eval(f.body, closureEnv + (f.param -> av),k)
    
    def eval[T](e: Exp, env: Env, k: Value =>T) : T = e match {
      case Num(n: Int) => k(NumV(n))
      case Id(x) => k(env(x))
      case Add(l,r) => eval(l,env, addc1(r,env,k))
      case f@Fun(param,body) => k(ClosureV(f, env))
      case App(f,a) => eval(f,env, evalc1(a,env,k))
    }
}

object Defunctionalized {
    sealed abstract class Value
    type Env = Map[Symbol, Value]
    case class NumV(n: Int) extends Value
    case class ClosureV(f: Fun, env: Env) extends Value
    
    sealed abstract class FunctionValue[T]
    case class AddC1[T](r: Exp, env: Env, k: FunctionValue[T]) extends FunctionValue[T]
    case class AddC2[T](lv: Value, k: FunctionValue[T]) extends FunctionValue[T]
    case class EvalC1[T](a: Exp, env: Env, k: FunctionValue[T]) extends FunctionValue[T]
    case class EvalC2[T](f: Fun, closureEnv: Env, k: FunctionValue[T]) extends FunctionValue[T]
 
    def apply[T](fv: FunctionValue[T], v: Value) : T  = fv match {
     case AddC1(r,env,k) => eval(r,env, AddC2(v,k))
     case AddC2(lv,k) => (lv,v) match {
                case (NumV(v1), NumV(v2)) => apply(k, NumV(v1+v2))
                case _ => sys.error("can only add numbers") 
         }
     case EvalC1(a,env,k) => v match {
                case ClosureV(f,closureEnv) => eval(a,env, EvalC2(f, closureEnv, k))
                case _ => sys.error("can only apply functions") }
     case EvalC2(f,closureEnv,k) => eval(f.body, closureEnv + (f.param -> v), k)
    }     
    def eval[T](e: Exp, env: Env, k: FunctionValue[T]) : T = e match {
      case Num(n: Int) => apply(k,NumV(n))
      case Id(x) => apply(k, env(x))
      case Add(l,r) => eval(l,env, AddC1(r,env,k))
      case f@Fun(param,body) => apply(k, ClosureV(f, env))
      case App(f,a) => eval(f,env, EvalC1(a,env,k))
    }
}