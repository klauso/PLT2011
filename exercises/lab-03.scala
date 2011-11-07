/*
Programming Languages and Types

Lab Session, November 07, 2011


Topics for today:

  * home work discussion
  * Language primitives
  * Infinite lists

*/

// Home Work

/*
 * (i)  Discuss the advantages and disadvantes of pattern matching versus the visitor pattern.
 * (ii) Discuss: free variables in WAE. Lookup "test-driven development" on the internet.
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




// Infinite lists


/*
Write a Haskell program that constructs a list of:

(i) all natural numbers
(ii) all even natural numbers
(iii) all squared integers
(iv) all factorials, i.e. the infinite sequence `1,1,2,6,24,120,720,...`

Discuss why infinite lists are useful.
*/
