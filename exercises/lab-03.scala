/*
Programming Languages and Types

Lab Session, November 07, 2011


Topics for today:

  * home work discussion
  * Environments and closures
  * Language primitives

*/

// Home Work


// Environments and closures

/*

i) In the environment-based interpreted we introduced closures. 
   What is a closure? What do we need them for?
ii) Why did we not need closures in the substitution-based interpreter?
iii) Why did we not need closures in the environment-based F1WAE interpreter?


*/

// Language primitives

/*
 * 
 * It is unsatisfactory to have many language primitives (such as `if0`).
 * It would be more elegant if there would just be a number of "pre-installed"
 * functions that are called using normal function application.

 * Let's say you define a function `myif0` as a F1WAE-function of three parameters
 * (ignore the problem that our functions can only have one parameter). The parameters
 * are `if-part`, `then-part` and `else-part`, and `(myif0 if-part then-part else-part)`
 * is just implemented as `(if0 if-part then-part else-part)`.

 * Think about whether you could use `myif0` rather than the builtin `if0` in your
 * factorial function. Explain why or why not. If not, think about a possible fix
 * that would let you use `myif0`.
 */



