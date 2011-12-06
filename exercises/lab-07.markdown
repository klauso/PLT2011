Programming Languages and Types

Lab Session, December 5, 2011


Home work
=========


First-class continuations
=========================

1. Illustrate first-class continuations by at least two examples.
2. Lambda-lift the following expression:
    fold((f: Int => Int, g: Int => Int) => (z: Int) => f(g(z)))
        ((z: Int) => z)
        (List(x => x+1, x => x+2, x => x-1, x => x*2))
3. Investigate the expressiveness of the following scenarios and how these
  setups differ:
  a. direct-style interpreter, direct-style program
  b. direct-style interpreter, CPS-transformed program
  b. CPS-transformed interpreter, direct-style program
  b. CPS-transformed interpreter, CPS-transformed program
