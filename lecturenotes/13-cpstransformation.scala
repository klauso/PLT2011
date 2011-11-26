/*
These are lecture notes for the "Programming Languages and Types" course by Klaus Ostermann 
at the University of Marburg

loosely based on Sec. 17 of "Programming Languages: Application and Interpretation" by
Shriram Krishnamurthi

Please comment/correct/improve these notes via github. Proposals or questions can
be submitted as an "issue"; proposals for corrections/extensions/improvements can
be submitted as a "pull request". You can of course also send an email to Klaus Ostermann */


/* Consider the following very simple program. */
def inputNumber(prompt: String): Int = {
  println(prompt)
  Integer.parseInt(readLine())
}
def progSimple = {
  println(inputNumber("First number:" ) + inputNumber("Second number"))
}
/* Now consider a web version of the program: 
 * - the first number is entered on the first page
 * - then the first number is submitted, a form to enter the second number is shown
 * - when the second number is submitted, a form with the result is generated and shown.
 *
 * Even this “addition server” is difficult to implement. Because http is a stateless protocol (for good reasons),
 * every web program is basically forced to terminate after every request. This means that 
 * the Web developer must turn this application into three programs:
 * (a) The first program displays the first form.
 * (b) The second program consumes the form values from the first form, and generates the second form.
 * (c) The third program consumes the form values from the second form, computes the output, and
 * generates the result.
 *
 * Because the value entered in the first form is needed by the third program to compute its output, this
 * value must somehow be transmitted between from the first program to the third. This is typically done
 * by using the hidden field mechanism of HTML.
 * 3. Suppose, instead of using a hidden field, the application developer used a Java Servlet session object,
 * or a database field, to store the first number. (Application developers are often pushed to do this
 * because that is the feature most conveniently supported by the language and API they are employing.)
 * Then, if the developer were to exploratorily open multiple windows
 * the application can compute the wrong answer. 
 
 * Hence, the actual program a web developer has the structure of the following program.
 * An actual web program is of course more complicated, but this primitive model of web
 * programming is sufficient to explain the problem. */
 
def webdisplay(s: String) : Nothing = { // we use the return type "Nothing" for functions that will never return (normally)
  println(s)
  sys.error("program terminated")
}

def webread(prompt: String, continue: String) : Nothing = {
  println(prompt)
  println("send input to: "+continue)
  sys.error("program terminated")
}

def program1 = webread("enter first number", "s2")
def program2(n1: Int) = webread("enter second number and remind me of previoulsy entered number "+n1, "s3")
def program3(n1: Int, n2: Int) = webdisplay("The sum of "+n1+" and "+n2+" is "+(n1+n2))

/* We could write a better webread and webdisplay procedure if we could somehow get hold of the
 * pending computation at the time when the procedure was invoked. Such a pending computation is
 * called _continuation_.
 *
 * Let's consider the continuation at the point of the first interaction in the original program,
 *
  def prog = {
    println(inputNumber() + inputNumber())  
  }
 * 
 * The pending computation (or, continuation in the following) is to take the result of the first
 * inputNumber invocation, add to it the result of the second invocation, and display it.
 * We can express the continuation as a function: */

val cont1 = (n: Int) => println(n + inputNumber("Second number"))

/* Similarly, the continuation at the second point of interaction is: 
 * val cont2 = (m: Int) => println(n+m)
 * where n is the result of the first input, which is stored in the closure of cont2
 *
 * Assuming an explicit representation of the continuation, it is obvious that we can now 
 * write a version of webread, called webread_k, which takes the continuation as second parameter and invokes
 * the continuation once the answer to the result is received by the server.
 * This can be implemented, say, by associating to a continatuion a unique ID, store the continuation
 * in a hashmap on the server using this ID, and storing the ID in the form such that it gets
 * submitted back to the server once the client presses the submit button.
 *
 * Here is code illustrating the idea. For simplicity we assume that all web forms return 
 * a single integer value.
 */
 
 
val continuations = new scala.collection.mutable.HashMap[Int, Int=>Nothing]()
var nextId : Int = 0
def getNextID = {
  nextId += 1
  nextId
}

def webread_k(prompt: String, k: Int=>Nothing) : Nothing = {
  val id = getNextID
  continuations += (id -> k)
  println(prompt)
  println("to continue, invoke continuation: "+id)
  sys.error("program terminated")
}

def continue(kid: Int, result: Int) = continuations(kid)(result)  

/* Using webread_k, we can now define our addition server as follows.
 * If you try prog, ignore the stack traces.
 */

def webprog = webread_k("enter first number", (n) =>
             webread_k("enter second number", (m) => webdisplay("The sum of "+n+" and "+m+" is "+(n+m))))

/* For instance, try:
 * scala> webprog          -- yields some continuation id c1
 * scala> continue(c1,5)   -- yields some continuation id c2  
 * scala> continue(c2,7)  
 *
 * This should yield the result 12 as expected. But also try:
 * scala> webprog          -- yields some continuation id c1
 * scala> contine(c1,5)    -- yields some continuation id c2
 * scala> contine(c1,6)    -- yields some continuation id c3
 * scala> continue(c2,3)   -- should yield 8
 * scala> continue(c3,3)   -- should yield 9
 *
 * The style of programming in webprog, which is obviously more complicated than the logical 
 * structure of progSimple, shows up in many practical programming scenarios - server-side web programming
 * is just one of them. For instance, in JavaScript many API functions for asynchronous communication
 * such as httprequest in AJAX require to pass a callback function, which leads to similar code
 * as the one in webprog above.
 *
 * Let's consider this "web transformation" - the transformation from progSimple to webprog - in more detail.
 * To this purpose, let's make our application a bit more sophisticated. Instead of entering only two
 * numbers, the user enters n numbers, e.g., the prices of a list of n items.
 *
 * Here is the "non-web" version of the application:
 */             
 
def allCosts(itemList: List[String]) : Int = itemList match {
   case Nil => 0
   case x :: rest => inputNumber("Cost of item "+x+":") + allCosts(rest)
}
val testData : List[String] = List("banana", "apple", "orange")

def test = println("Total sum: "+allCosts(testData))

/* This version of allCosts is clearly not web-friendly, because it uses inputNumber,
 * which we do not know how to implement for the web.
 * The first thing to observe is that on its own, tally is not a complete program: it doesn’t do anything!
 * Instead, it is a library procedure that may be used in many different contexts. Because it has a Web inter-
 * action, however, there is the danger that at the point of interaction, the rest of the computation—i.e., the
 * computation that invoked allCosts — will be lost. To prevent this, allCosts must consume an extra argument, 
 * a continuation, that represents the rest of the pending computation. To signify this change in contract, we will use
 * the convention of appending _k to the name of the procedure and k to name the continuation parameter.
 */ 
 
/* Here is a first attempt to define allCosts_k.
 *

 def allCosts_k(itemList: List[String], k: Int => Nothing) : Nothing = itemList match {
   case Nil => 0
   case x :: rest => webread_k("Cost of item "+x+":", n => n+allCosts_k(rest,k)) 
}

 * This may look reasonable, but it suffers from an immediate problem. When the recursive call occurs, if
 * the list had two or more elements, then there will immediately be another Web interaction. Because this will
 * terminate the program, the pending addition will be lost! Therefore, the addition of n has to move into the
 * continuation fed to allCosts_k. In code:
 
def allCosts_k(itemList: List[String], k: Int => Nothing) : Nothing = itemList match {
   case Nil => 0
   case x :: rest => webread_k("Cost of item "+x+":", n => allCosts_k(rest,m => k(m+n))) 
}
 * That is, the receiver of the Web interaction is invoked with the cost of the first item. When allCosts_k is invoked
 * recursively, it is applied to the rest of the list. Its receiver must therefore receive the tally of costs of the
 * remaining items. That explains the pattern in the receiver.
 * The only problem is, where does a continuation ever get a value? We create larger-and-larger continuation on
 * each recursive invocation, but what ever invokes them?
 * Here is the same problem from a different angle (that also answers the question above). Notice that each
 * recursive invocation of allCosts_k takes place in the aftermath of a Web interaction. We have already seen how
 * the act of Web interaction terminates the pending computation. Therefore, when the list empties, where
 * is the value 0 going? Presumably to the pending computation—but there is none. Any computation that
 * would have been pending has now been recorded in k, which is expecting a value. Therefore, the correct
 * transformation of this procedure is:
 */
 
def allCosts_k(itemList: List[String], k: Int => Nothing) : Nothing = itemList match {
   case Nil => k(0)
   case x :: rest => webread_k("Cost of item "+x+":", n => allCosts_k(rest,m => k(m+n))) 
}

def testweb = allCosts_k(testData, m => webdisplay("Total sum: "+m))

/* Let's now consider the case that we have used a library function for the iteration in allCosts, namely
 * the map function, which we replicate here. */
 def map[S,T](c: List[S], f: S=>T) : List[T] = c match {
   case Nil => Nil
   case x :: rest => f(x) :: map(rest,f) 
}
/* Using map, we can rephrase allCosts as follows. */
def allCosts2(itemList: List[String]) : Int = map(itemList, (x:String) => inputNumber("Cost of item "+x+":")).sum

/* What we we want to use map in allCosts_k? 
 * Just using webread_k in place of inputNumber will not work, since webread_k expects an additional parameter.
 * Obviously we must somehow modify the definition of map. But what can we pass as second parameter in the call
 * to f? 
 *
 * Insight: We must perform the "web transformation" to map as well. We call the reslt map_k. Here is the code: */
 
def map_k[S,T](c: List[S], f: (S,T=>Nothing) => Nothing, k: List[T] => Nothing) : Nothing = c match {
   case Nil => k(Nil)
   case x :: rest => f(x, t => map_k(rest, f, (tr:List[T]) => k(t::tr)))     
}

def allCosts2_k(itemList: List[String], k: Int => Nothing) : Nothing = 
   map_k(itemList, (x:String,k2:Int=>Nothing) => webread_k("Cost of item "+x+":", k2),(l:List[Int]) => k(l.sum)) 
}

/* Implications of the "web transformation": 
 * 
 * 1. We have had to make decisions about the order of evaluation. That is, we had to choose whether
 * to evaluate the left or the right argument of addition first. This was an issue we had specified only
 * implicitly earlier; if our evaluator had chosen to evaluate arguments right-to-left, the Web program at
 * the beginning of this document would have asked for the second argument before the first! We have
 * made this left-to-right order of evaluation explicit in our transformation.
 * 
 * 2. The transformation we use is global, namely it (potentially) affects all the procedures in the program
 * by forcing them all to consume an extra continuation as an argument. We usually don’t have a choice as
 * to whether or not to transform a procedure. Suppose f invokes g and g invokes h, and we transform
 * f to f_k but don’t transform g or h. Now when f_k invokes g and g invokes h, suppose h consumes
 * input from the Web. At this point the program terminates, but the last continuation procedure (necessary
 * to resume the computation when the user supplies an input) is the one given to f_k, with all record of
 * g and h erased.
 * 
 * 3. This transformation sequentializes the program. Given a nested expression, it forces the programmer
 * to choose which sub-expression to evaluate first (a consequence of the first point above); further, every
 * subsequent operation lies in the continuation, which in turn picks the first expression to evaluate, pushing
 * all other operations into its continuation; and so forth. The net result is a program that looks an awful lot
 * like a traditional procedural program. This suggests that this series of transformations can be used to
 * compile a program in a language like Scheme into one in a language like C! 
 */