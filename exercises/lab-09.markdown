Programming Languages and Types

Lab Session, January 9, 2012


Polymorphism
============

A function is _polymorphic_, if it applies to arguments of different types.
After Strachey (1967), there are two kinds of polymorphism:

 * ad-hoc:  a function behaves differently for arguments of different types
 * parametric: a function behaves equally for arguments of different type

Parametric polymorphism is modeled by parametric types:

 * `length :: [a] -> Int`
 * `map :: (a -> b) -> [a] -> [b]`

Discuss how ad-hoc polymorphism is modeled in existing languages, e.g., C++,
Java, Haskell. What is the type of a user-defined multiplication function
`mult`? What is the type of usages of this function, for example, `square x = x
* x`.


