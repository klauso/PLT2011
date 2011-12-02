sealed abstract class Exp
case class Num(n: Int) extends Exp
case class Id(name: Symbol) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Fun(param: Symbol, body: Exp) extends Exp
implicit def num2exp(n: Int) = Num(n)
implicit def id2exp(s: Symbol) = Id(s)
case class App (funExpr: Exp, argExpr: Exp) extends Exp

sealed abstract class Value
type Env = Map[Symbol, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value

trait Monad[M[_]] {
  def unit[A](a: A): M[A]
  def bind[A, B](m: M[A])(f: A => M[B]): M[B]
}

implicit def forCompSyntax[M[_], A](m: M[A])(implicit tc: Monad[M]) = new {
  def map[B](f: A => B) = tc.bind(m)( a => tc.unit(f(a)))
  def flatMap[B](f: A => M[B]) = tc.bind(m)(f)
}

implicit object MonadicOption extends Monad[Option] {
  def unit[A](a: A) = Some(a)
  def bind[A, B](opt: Option[A])(f: A => Option[B]) = opt flatMap f
}
trait Reader[I, O] {  
  def apply(c: I) : O
}

trait PartialTypeApply[T[_, _], A] {
 type X[B] = T[A, B]
}
class ReaderMonad[I] extends Monad[PartialTypeApply[Reader,I]#X]{ 
  def unit[O](o: O) =  new Reader[I, O] { def apply(c: I) = o }
  def bind[O,Q](a: Reader[I,O])(f: O => Reader[I,Q]) = new Reader[I,Q]{ def apply(c: I) = f(a(c))(c) }
  def ask : Reader[I,I] = new Reader[I,I] { def apply(c: I) = c }
  def local[O](f: I => I)(r: Reader[I,O]) : Reader[I,O] = new Reader[I,O] { def apply(c: I) = r(f(c)) }
} 

implicit def rm[R] : ReaderMonad[R] = new ReaderMonad()

sealed abstract class Value
type Env = Map[Symbol, Value]
case class NumV(n: Int) extends Value
case class ClosureV(f: Fun, env: Env) extends Value

def eval(e: Exp): Reader[Env,Value] = e match {
  case Num(n) => rm.unit(NumV(n))
  case Id(x) => for (env <- forCompSyntax[PartialTypeApply[Reader,Env]#X,Env](rm.ask)) yield env(x)
  // This is what I want to write:
  //case Id(x) => for (env <- rm.ask) yield env(x)
  case Add(l,r) => sys.error("not yet")
  case Fun(x,body) => sys.error("not yet")
  case App(f,a) => sys.error("not yet")
}