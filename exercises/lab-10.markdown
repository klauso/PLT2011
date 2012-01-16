Programming Languages and Types

Lab Session, January 16, 2012


Typing derivations
============

Explicitly write down one possible typing derivation of the term `\a. \b. \c. b
(a b c)` from the simply typed lambda calculus.


Type safety
==========

Consider the reduction and type system for arithmetics and booleans from last
week. We want to proof that this type system is sound, however, to make things
more interesting let's add the following construct.

    t ::= error

    error op t -> error,  for op in {and, or}
    t op error -> error,  for op in {and, or}
    f error -> error,     for f  in {succ, pred, iszero}

To support `error`, the type system needs to be extended as well. How?

Verify the soundness of the resulting type system.
