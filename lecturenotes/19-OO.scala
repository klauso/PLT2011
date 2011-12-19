/* This is the definition of mini OO language we designed in class on December 19, 2011.
 * I've added a small test case which also illustrates how one can encode boolean
 * arithmetic with pure objects. This encoding is not purely hypothetic: Pure OO
 * languages such as Smalltalk do model booleans like this.
 *
 * Recommended exercises for understanding this language:
 * - translate the example program to a program in an ordinary OO language (such as Java)
 * - invent new interesting test cases
 * - add a few features to this language (such as: local variables, mutable fields, multiple inheritance, ...)
 * - create an environment-based version of this interpreter
 * - Advanced exercise: Look at the interpreter for real-world OO languages such as Ruby and compare with our interpreter
 */

sealed abstract class Exp
case class New(name: Symbol, args: List[Exp]) extends Exp
case class GetField(e: Exp, fieldName: Symbol) extends Exp
case class MethodCall(e: Exp, methodName: Symbol, args: List[Exp]) extends Exp
case class Id(x: Symbol) extends Exp

case class Class(name: Symbol,
                 superClass: Symbol, 
                 fields: List[Symbol], 
                 methods: Map[Symbol, (List[Symbol], Exp)])
                
def allFields(classes: List[Class], className: Symbol) : List[Symbol] = {
  className match {
    case 'Object => List()
    case cn => classes.find( c => c.name == cn) match {
        case Some(cl) => allFields(classes, cl.superClass) ++ cl.fields
        case None => sys.error("class not found: "+cn)        
    }
  }
}

def lookupMethod(classes: List[Class], className: Symbol, methodName: Symbol) : (List[Symbol], Exp)  = {
  classes.find( c => c.name == className) match {
    case Some(cl) => cl.methods.get(methodName) match {
      case Some(mthd) => mthd
      case None => cl.superClass match {
         case 'Object => sys.error("Method not found: "+methodName)
         case sc => lookupMethod(classes, sc, methodName) 
      }
    }
    case None => sys.error("class not found: "+className) 
  }    
}

def subst(e: Exp, substitutions: Map[Symbol, Exp]) : Exp = e match {
  case Id(x) => substitutions.getOrElse(x, e)
  case New(name, args) => New(name, args.map(subst(_, substitutions)))
  case GetField(e, fieldName) => GetField(subst(e,substitutions), fieldName)
  case MethodCall(e, methodName, args) => 
    MethodCall(subst(e,substitutions), methodName, args.map(subst(_, substitutions)))
}
                  
def eval(classes : List[Class], exp: Exp) : Exp  = exp  match {
  case Id(x) => sys.error("unknown identifier: "+x)
  case New(name, args) => 
      New(name,  args.map(eval(classes,_)))
  case GetField(e, fieldName) => eval(classes,e) match {
    case New(cn, fields) => (Map.empty ++ allFields(classes, cn).zip(fields))(fieldName)
    case _ => sys.error("Unevaluated expression")
  }
  case MethodCall(e, methodName, args) => eval(classes,e) match {
    case newthis@New(cn, fields) => {
      val method = lookupMethod(classes, cn, methodName)
      eval(classes,
           subst(method._2,  Map('this -> newthis)  ++ method._1.zip(args.map(eval(classes,_)))))
    }
    case _ => sys.error("Unevaluated expression")
  } 
}   

val testclasses = List(
  Class('True, 'Object, List.empty, Map(
     'ifThenElse -> (List('thenExp, 'elseExp), Id('thenExp)),
     'and -> (List('x), Id('x)))),
  Class('False, 'Object, List.empty, Map(
     'ifThenElse -> (List('thenExp, 'elseExp), Id('elseExp)),
     'and -> (List('x), Id('this)))),
  Class('Food, 'Object, List('organic),  Map(
     'tastesBetterThan -> 
       (List('other), MethodCall(GetField(Id('this), 'organic), 'ifThenElse, List(New('True, List.empty), GetField(Id('otherFood), 'organic)))))),
  Class('Pizza, 'Food, List('hasCheese), Map(
     'tastesBetterThan -> 
       (List('other), MethodCall(GetField(Id('this), 'organic), 'and,        List(GetField(Id('this), 'hasCheese)))))))
     
assert( 
 eval(testclasses,  
   MethodCall(
     New('Pizza, List(New('True, List.empty), New('True, List.empty))),
     'tastesBetterThan,
     List(New('Food, List(New('True, List.empty))))))
  ==
  New('True, List.empty))
               