Programming Languages and Types

Lab Session, December 12, 2011


Home work
=========

Monadic laws
============

To qualify as a monad, one does not only need a type constructor and a
bind/return implementation of the correct type. In addition, monads must
respect the following three algebraic laws:

1. `(bind (return x) f)  == (f x)`
2. `(bind m return) == m`
3. `(bind (bind m f) g) == (bind m (lambda (x) (bind (f x) g)))`

1. Rephrase the laws in terms of do-notation. 
2. Prove that the identity monad, the Maybe monad, and the Reader monad satisfy these laws. 
3. Discuss why monads should obey these laws.
