/* This is the statically typed version of the mini OO language we designed in class on December 19, 2011 (see 19-OO.scala)
 * The type system was discussed in class on January 24, 2012.
 *
 * Comments and explanations will be added later. For now, please experiment with the type checker.
 */

sealed abstract class Exp
case class New(name: Symbol, args: List[Exp]) extends Exp
case class GetField(e: Exp, fieldName: Symbol) extends Exp
case class MethodCall(e: Exp, methodName: Symbol, args: List[Exp]) extends Exp
case class Id(x: Symbol) extends Exp

case class MethodDecl(args: List[(Symbol,Symbol)], body: Exp, retType: Symbol)
case class Class( // name: Symbol,
                 superClass: Symbol, 
                 fields: List[(Symbol,Symbol)],  // first component: field name, second component: field type
                 methods: Map[Symbol, MethodDecl])
                
def allFields(classes: Map[Symbol,Class], className: Symbol) : List[(Symbol,Symbol)] = {
  className match {
    case 'Object => List.empty
    case cn => classes.get(cn) match {
        case Some(cl) => allFields(classes, cl.superClass) ++ cl.fields
        case None => sys.error("class not found: "+cn)        
    }
  }
}

// returns list of (type, argname) pairs, body, and return type
def lookupMethod(classes: Map[Symbol,Class], className: Symbol, methodName: Symbol) : Option[MethodDecl]  = {
  if (className == 'Object) None else 
      classes.get(className) match {
        case Some(cl) => cl.methods.get(methodName) match {
          case Some(mthd) => Some(mthd)
          case None => lookupMethod(classes, cl.superClass, methodName) 
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
                  
def eval(classes : Map[Symbol,Class], exp: Exp) : Exp  = exp  match {
  case Id(x) => sys.error("unknown identifier: "+x)
  case New(name, args) => 
      New(name,  args.map(eval(classes,_)))
  case GetField(e, fieldName) => eval(classes,e) match {
    case New(cn, fieldValues) => (Map.empty ++ allFields(classes, cn).map(_._1).zip(fieldValues))(fieldName)
    case _ => sys.error("Unevaluated expression")
  }
  case MethodCall(e, methodName, args) => eval(classes,e) match {
    case newthis@New(cn, fields) => {
      lookupMethod(classes, cn, methodName) match {
        case Some(method) =>  
           eval(classes,
             subst(method.body,  Map('this -> newthis)  ++ method.args.map(_._1).zip(args.map(eval(classes,_)))))
        case None => sys.error("Method not found: "+methodName)
      }
    }
    case _ => sys.error("Unevaluated expression")
  } 
}   

def subtype(classes: Map[Symbol, Class], c1: Symbol, c2: Symbol) : Boolean = 
  if (c1 == c2) true else 
    if (c1 == 'Object) false else 
       subtype(classes, classes(c1).superClass, c2)

def typecheck(classes: Map[Symbol, Class], env: Map[Symbol, Symbol], exp: Exp) : Symbol = exp match 
{
  case Id(x) => env.get(x) match {
    case Some(c) => c
    case None => sys.error("unknown identifier: "+x + " in env: "+env)
  }    
  case New(name, args) => classes.get(name) match {
    case Some(cl) => {
      val argtypes = args.map(typecheck(classes,env,_))
      if (args.length != argtypes.length) sys.error("wrong number of arguments") 
      argtypes.zip(allFields(classes, name).map(_._2)).
        foreach( (s: (Symbol,Symbol)) => if (! subtype(classes, s._1, s._2)) sys.error("Actual arg type "+s._1+" is not a subtype of formal arg type "+s._2))
      name
    }    
    case None => sys.error("unknown class: "+name)
  }    
  case GetField(e, fieldName) => (Map.empty++allFields(classes,typecheck(classes,env,e))).get(fieldName) match {
    case Some(c) => c
    case None => sys.error("field "+fieldName+" does not exist")
  }
  case MethodCall(e, methodName, args) => {
    val rect = typecheck(classes, env, e)
    val argt = args.map(typecheck(classes,env,_))
    classes(rect).methods.get(methodName) match {
      case Some(methodDecl) => {
        if (methodDecl.args.length != args.length) sys.error("wrong number of arguments in call to "+methodName)
        argt.zip(methodDecl.args.map(_._2)).foreach(
          (s: (Symbol,Symbol)) => if (! subtype(classes, s._1, s._2)) 
             sys.error("Actual arg type "+s._1+" is not a subtype of formal arg type "+s._2))        
        methodDecl.retType
      }
      case None => sys.error("unknown method: "+methodName)
    }
  }
}
def checkClassExists(classes: Map[Symbol,Class], className: Symbol) : Unit = {
  if ((className != 'Object) && (!classes.contains(className))) sys.error("unknown class: "+className)
}
def checkmethod(classes: Map[Symbol,Class], className: Symbol, method: Symbol, md: MethodDecl) = {
  checkClassExists(classes,md.retType)
  md.args.foreach((s:(Symbol,Symbol)) => checkClassExists(classes, s._2))
  lookupMethod(classes, classes(className).superClass, method) match {
    case Some(smd) => if ((smd.retType != md.retType) || (smd.args.map(_._2) != md.args.map(_._2))) 
      sys.error("Invalid overriding of method: "+method+ " in class : "+className)
    case None => ()
  }
  if (!subtype(classes,typecheck(classes, Map('this -> className)++md.args, md.body), md.retType))
    sys.error("Type of method body for method: "+method+" is not subtype of "+md.retType+" in class: "+className)
  
}
def checkclass(classes: Map[Symbol,Class], className: Symbol, cd: Class) = {
  checkClassExists(classes, cd.superClass)
  cd.fields.foreach((s: (Symbol,Symbol)) => checkClassExists(classes,s._2))
  cd.methods.foreach((m:(Symbol, MethodDecl)) => checkmethod(classes, className, m._1, m._2))
  
}
def checkprog(classes: Map[Symbol,Class])  = classes.foreach((s: (Symbol,Class)) => checkclass(classes,s._1, s._2))
   
// def subtype(c1: Symbol, c2: Symbol, 
val testclasses = Map(
  'Bool -> Class('Object, List.empty, Map(
     'ifThenElse -> MethodDecl( List(('thenExp, 'Object), ('elseExp, 'Object)), Id('thenExp), 'Object), // dummy default implementation
     'and -> MethodDecl(List(('x,'Bool)), Id('x), 'Bool))),                                             // dummy default implementation
  'True -> Class('Bool, List.empty, Map(
     'ifThenElse -> MethodDecl( List(('thenExp, 'Object), ('elseExp, 'Object)), Id('thenExp), 'Object),
     'and -> MethodDecl(List(('x,'Bool)), Id('x), 'Bool))),
  'False -> Class('Bool, List.empty, Map(
     'ifThenElse -> MethodDecl(List(('thenExp, 'Object), ('elseExp, 'Object)), Id('elseExp), 'Object),
     'and -> MethodDecl(List(('x, 'Bool)), Id('this), 'Bool))),
  'Food -> Class('Object, List(('organic, 'Bool)),  Map(
     'tastesBetterThan -> 
       MethodDecl(List(('other, 'Food)), GetField(Id('this), 'organic), 'Bool))),
  'Pizza -> Class('Food, List(('hasCheese,'Bool)), Map(
     'tastesBetterThan -> 
       MethodDecl(List(('other,'Food)), MethodCall(GetField(Id('this), 'organic), 'and,        List(GetField(Id('this), 'hasCheese))), 'Bool))))

checkprog(testclasses)
       
assert( 
 eval(testclasses,  
   MethodCall(
     New('Pizza, List(New('True, List.empty), New('True, List.empty))),
     'tastesBetterThan,
     List(New('Food, List(New('True, List.empty))))))
  ==
  New('True, List.empty))
