import Data.Map 


-- An explanations of today's lecture notes are also in Sec. 1 and 2 of the following paper:
-- The essence of functional programming, Philip Wadler,  19'th Symposium on Principles of Programming Languages, ACM Press, Albuquerque, January 1992. 
-- available at http://homepages.inf.ed.ac.uk/wadler/papers/essence/essence.ps


{-
_Monads_ are an abstraction of how functions are composed. In ordinary function application,
the result of some function application is passed as an argument to another function, hence
ordinary function application has the type and implementation

bind :: a -> (a -> b) -> a
bind a f = f a

In many cases, however, function composition is more sophisticated but still equally uniform.
It turns out that many interesting kinds of function composition have the form

bind :: m a -> (a -> m b) -> m b

for some type constructor m, such as the Maybe or the list type constructor. In addition, it
is useful to have a way to "inject" an ordinary value into such a type constructor, i.e.,
a function

return :: a -> m a

With return, one can, for instance, define a variant of bind that accepts a function of
type a -> b rather than a -> m b as follows:

map :: m a -> (a -> b) -> m b
map x f = x `bind` \y -> return (f x)

(Note that this function is a generalization of the map function on lists)

Monads can be defined and used in any programming language, but in some languages the necessary
syntactic overhead makes programming with monads less attractive. Haskell is a language
in which it is particularly convenient to program with monads, hence we choose Haskell to illustrate
the idea.

In Haskell, the Monad interface from above is described as a type class (find out what type classes
are if you do not know). This type class has the "bind" and "return" functions in its interface, and
also some convenience methods such as ">>", which are useful if the result of the previous function
call is irrelevant. The bind function is written ">>=" in Haskell (but still pronounced 'bind').
 
class  Monad m  where
    (>>=)       :: m a -> (a -> m b) -> m b
    return      :: a -> m a
    m >> k      =  m >>= \_ -> k


This typeclass is useful to write code that is polymorphic in the actual monad, such as the library function
 
mapM :: Monad m => (a -> m b) -> [a] -> m [b]    

A similar effect can be achieved in other languages by passing the ">>=" and "return" 
functions as parameters to monad-polymorphic functions. It is however difficult to do
so in a type-safe way if the language is statically typed but does not support
parameterization by type constructors.

In the remainder of these notes we will study several variants of our interpreters
using monadic style. The problem that we want to address with monads is that the interpreters
were rather fragile with regard to certain changes, such as introducing environments or mutable state.
In particular, the definitions of language constructs that are irrelevant from the point of view
of the new feature had to be changed, e.g., we had to change the "Add" branch of the interpreter
when we introduced environments or mutable state. This is bad in several ways: a) We would rather
have a more modular way of specifying the semantics of our language constructs, in which the 
semantic definitions of the language constructs are decoupled from each other. b) It is bad
from a software engineering point of view because we have to change a lot of code (basically
the whole interpreter, which could be huge for realistic languages) when we
introduce a new language feature. Historically, monads were invented (or "found") to address
problem a).

At first, let us consider the basic AE language from the
first language. Here is the syntax of expressions and values:
-}

data Exp1 = Num1 Int | Add1 Exp1 Exp1 deriving Show
data Value1 = NumV1 Int deriving (Show,Eq)

{- 
Here is the standard non-monadic evaluator: 
-}

eval :: Exp1 -> Value1
eval (Num1 n)   = NumV1 n
eval (Add1 l r) = 
  let (NumV1 v1) = eval l
      (NumV1 v2) = eval r
  in NumV1 (v1+v2)

{- We will now write the same evaluator in monadic style. This means that we
assume that the return type of the function is "m Value1" for some monad m,
rather than just Value1. This forces us to use >>= and return to compose
function calls, rather than using ordinary function composition. -}
    
eval1 :: Monad m => Exp1 -> m Value1
eval1 (Num1 n)   = return (NumV1 n)
eval1 (Add1 l r) = 
  eval1 l >>= (\(NumV1 v1) -> 
  eval1 r >>= (\(NumV1 v2) -> 
  return (NumV1 (v1+v2))))

  
{- This style of nesting functions can become somewhat cumbersome. For this reason
Haskell provides syntactic sugar for using ">>=", namely the _do notation_.
Using do-notation we can rewrite eval1 to eval1' as follows: -}
 
-- eval1' is identical to eval1 but uses do-notation   
eval1' :: Monad m => Exp1 -> m Value1
eval1' (Num1 n)   = return (NumV1 n)
eval1' (Add1 l r) = 
 do (NumV1 v1) <- eval1' l
    (NumV1 v2) <- eval1' r
    return (NumV1 (v1+v2))

{- 
The desugaring rules for do-notation are as follows: 
do { x } = x
do { x ; <stmts> }
  = x >> do { <stmts> }
do { v <- x ; <stmts> }
  = x >>= \v -> do { <stmts> }

To use eval1 or eval1', we have to specify a monad with which we want
to parameterize the interpreter. In Haskell, we can define a monad
by defining an instance of the Monad typeclass. An instance is
passed to Monad-polymorphic functions implicitly, based on the type
context in which the function is called. In other languages, one would
pass the monad instance explicitly.

The simplest monad is the _identity monad_, which is a kind of neutral
element in the sense that it means that we want just ordinary function 
composition.

The type constructor for the identity monad is the identity function on types,
lambda T.T
Since Haskell does not support proper functions on the type level,  we have
to use a data type definition instead, which means that "m a" is not "a"
but "Identity a" for some data type "a". This makes using the identity monad
a bit more cumbersome since we have to wrap and unwrap the "Identity a" values. 

Usually these data types are of the form "data X a = ...". In the special case
that the data type has exactly one variant with exactly one field, we can 
use a "newtype" declaration instead. This is useful because the "newtype" construct
can be implemented more efficiently than "data".

Here is the definition of the identity monad:
-}  

newtype Identity a = Identity { runIdentity :: a } -- runIdentity extracts the stored value
                                                   -- its type is runIdentity :: Identity a -> a
instance Monad Identity where
   return a = Identity a
   m >>= k  = k (runIdentity m)

{- We can now use this monad to run our eval1. 
Notice that we unwrap the value with "runIdentity".
The Haskell compiler will implicitly pass the Identity monad to eval1 because it can infer
from the call to "runIdentity", an instance of "Identity" is needed, and hence the
only matching monad instance is the Identity Monad.
-}
   
test1 = Add1 (Num1 3) (Num1 5)
runtest1 :: Value1
runtest1 = runIdentity (eval1 test1)

{-
The identity monad illustrates that we can always get the "ordinary" behavior of a
monadic function back. But monads become interesting when we use different forms
of function composition. To this end, we will now introduce the _Maybe_ monad.

To motivate the Maybe monad, let us consider how we deal with errors in our interpreters,
such as "variable not bound", "Closure expected, found Number" etc.

We have often dealt with such errors using the exception handling facilities of the
meta-language, but exception handling is rather limited because the errors are
not "first class" - for instance, we cannot define an "error value" which is an
actual value. A better alternative is to encode possible errors in the type of
values. To this end, Haskell offers the standard datatype "Maybe"

-- data  Maybe a  =  Nothing | Just a

Similar types are available in other languages, e.g., in Scala Maybe is called "Option".

Since our language is so simple that no errors can occur, we extend it with a
new (useless) language construct whose semantics is that it produces a failure.

Using Maybe, we can define the language like this:
-}

data Exp2 = Num2 Int | Add2 Exp2 Exp2 | Fail2 deriving Show
data Value2 = NumV2 Int deriving (Show,Eq)

eval2 :: Exp2 -> Maybe Value2
eval2 (Num2 n) = Just (NumV2 n)
eval2 (Add2 l r) = 
   case eval2 l of
      Just (NumV2 v1) -> case eval2 r of 
             Just (NumV2 v2) -> Just (NumV2 (v1+v2))
             Nothing -> Nothing
      Nothing ->  Nothing
eval2 Fail2 = Nothing       

{- Here we see a typical example of the modularity problem mentioned above. Although
addition has little to do with the new "Fail" construct, we have to completely alter
its definition.

Luckily, the kind of function composition as displayed in the Add branch of the above
interpreter can be abstracted over in a monad: The Maybe monad. Here is how the
Maybe monad is defined in the Haskell standard library:
-}

-- instance  Monad Maybe  where
--    (Just x) >>= k      = k x
--    Nothing  >>= _      = Nothing
--    return              = Just
  
{- Using the Maybe monad, we can now write the interpreter for Exp2 as follows: -}
  
eval2' :: Exp2 -> Maybe Value2
eval2' (Num2 n)   = return (NumV2 n)
eval2' (Add2 l r) = 
 do (NumV2 v1) <- eval2' l
    (NumV2 v2) <- eval2' r
    return (NumV2 (v1+v2))
eval2' Fail2 = Nothing

{- 
Notice that the "Add2" branch is still exactly the same as in the original eval1' interpreter!
Yet its behavior is very different when used with the Maybe monad instead of the Identity monad:
When used with the identity monad it behaves like the add branch of eval; with the Maybe
monad it behaves like the add branch of eval2.

Here are some tests that illustrate that eval2 and eval2' implement the same semantics.
-}
test2 = Add2 Fail2 (Num2 5)
test2' = Add2 (Num2 3) (Num2 5)
runtest2 = eval2 test2 -- should be Nothing
runtest2' = eval2 test2' -- should be Just (NumV2 8)

{- Another important pattern of function composition which we observed in our
interpreters is the passing of environments in the environment-based interpreters.
This pattern is captured and generalized by the _reader_ (or environment) monad.

It abstracts over the type of environments (r) and values (a). The return
function of this monad ignores the environment, whereas the >>= function
captures the environment propagation pattern.
-}

newtype Reader r a = Reader {
    runReader :: r -> a
}

instance Monad (Reader r) where
    return a = Reader $ \_ -> a
    m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

{- The following two auxiliary functions are useful to get the current environment
(askR) and change the environment in a subcomputation (localR). In the standard
Haskell library, these functions are members of a type class MonadReader, but for
simplicity we do not abstract over the reader type here. -}   
askR :: Reader a a 
askR = Reader id

localR :: (r -> b) -> Reader b a -> Reader r a
localR f m = Reader $ runReader m . f

{- Using the reader monad, we can now specify the environment-based FAE interpreter.
Note that those cases unrelated to environments (Add and Num) are unaffected by
the addition of environments. -}

data Exp3 = Num3 Int | Id3 String | Add3 Exp3 Exp3 | Fun3 String Exp3 | App3 Exp3 Exp3 deriving (Show,Eq)
data Value3 = NumV3 Int | ClosureV3 String Exp3 Env3 deriving (Show,Eq)
type Env3 = Map String Value3

eval3 :: Exp3 -> Reader Env3 Value3
eval3 (Num3 n) = return $ NumV3 n 
eval3 (Add3 l r) = 
 do (NumV3 v1) <- eval3 l
    (NumV3 v2) <- eval3 r
    return (NumV3 (v1+v2))
eval3 (Id3 x) = 
  do env <- askR
     return (env ! x)
eval3 (Fun3 param body) = 
  do
    env <- askR
    return $ ClosureV3 param body env
eval3  (App3 f a) = 
  do
   (ClosureV3 param body cenv) <- eval3 f
   av <- eval3 a
   localR (\env -> (insert param av cenv)) (eval3 body   )

test3 = App3 (Fun3 "x" (Add3 (Id3 "x") (Num3 5))) (Num3 7)

runtest3 = (runReader $ eval3 test3) empty -- snhould be  NumV3 12

{- Another important monad is the _list monad_. Computations in the list monad (that is, return type [a]) 
represent computations with zero or more valid answers. 
Here is the instance declaration for the list monad:

instance Monad [] where
  return a = [a]
  xs >>= f = concat (map f xs)

The list monad has many real-world applications. It is the conceptual foundation of Microsoft's LINQ,
list comprehensions in Haskell, and for-comprehensions in Scala. In lazy languages, it is also useful
to implement backtracking algorithms in an elegant way.

We illustrate the list monad with a hypothetical extension of our language by "amb" expressions.
Amb(e1,e2) means that the result of this expression is nondeterministically either the value of e1 or of e2.
We write an interpreter that enumerates all possible values of a program containing "amb":
-}

data Exp4 = Num4 Int | Add4 Exp4 Exp4 | Amb4 Exp4 Exp4 deriving Show
data Value4 = NumV4 Int deriving (Show,Eq)

eval4 :: Exp4 -> [Value4]
eval4 (Num4 n)   = return (NumV4 n)
eval4 (Add4 l r) = 
 do (NumV4 v1) <- eval4 l
    (NumV4 v2) <- eval4 r
    return (NumV4 (v1+v2))
eval4 (Amb4 e1 e2) = eval4 e1 ++ eval4 e2

{- Another function composition pattern we have seen was the dataflow of store passing in the BCFAE interpreter.
This pattern of function composition is captured by the _state monad_.

Here is its definition: -}
newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return a = State $ \s -> (a, s)
    m >>= k  = State $ \s -> let
        (a, s') = runState m s
        in runState (k a) s'

{- Auxiliary functions for conveniently reading and writing the store -}        
getState   = State $ \s -> (s, s)
putState s = State $ \_ -> ((), s)        

{- We cannot yet implement BCFAE using this monad, because we also need to pass around the environment.
For now we will hence consider a simpler example. Let's assume we want to count the number of 
additions performed during evaluation of an AE expression. Our state is just a single integer
in this case, hence -}

eval1'' :: Exp1 -> State Int Value1
eval1'' (Num1 n)   = return (NumV1 n)
eval1'' (Add1 l r) = 
 do (NumV1 v1) <- eval1' l
    (NumV1 v2) <- eval1' r
    c <- getState
    putState (c+1)
    return (NumV1 (v1+v2))

{- A monad which is somewhat related to the state monad is the _IO monad_. A value
of type IO a is a computation that, when performed, may do some input/output before
delivering a value of type a.

A more concrete way of looking at the IO monad is to think of it as having 
a type similar to the state monad, except that the state is the whole external "World".

IO a = World -> (a, World)

It is important to understand that a value of type IO a just represents the "plan" to
perform some IO, which is different from actually executing the IO. Such a plan is only
executed when it is applied to some initial "world", which only happens in "main" or
the ghci prompt.

Another important point is that the actual definition of the IO type constructor is hidden,
and there is no way to get "out" of the IO monad, i.e., it is not possible to extract 
the a out of an IO a value (except if using "unsafePerformIO", which is forbidden
by the UN declaration of human rights).

Here are two typical functions using the IO monad:

getChar :: IO Char
putChar :: Char -> IO () 

The monad operations, bind and return, have the following meaning: The bind operation
composes (and sequentializes) the IO plans in the arguments; return lifts a value
into the IO monad without any IO effects associated to it.
    
For instance, to read to characters and return them into a pair of characters we can
write the following function: -}

getTwoChars :: IO (Char,Char) 
getTwoChars = 
  do
    c1 <- getChar
    c2 <- getChar
    return (c1,c2)
    
{- Since >>= is the only operation that composes IO actions, and since the IO type
is hidden, it is impossible for the program to ever get hold of a "world". The
"world" is never duplicated or thrown away.

The IO monad allows us to rebuild the control structures of ordinary imperative languages
as ordinary functions in the IO monad.

Examples:
-}    

forever :: IO () -> IO ()
forever a = a >> forever a

repeatN :: Int -> IO a -> IO ()
repeatN 0 a = return ()
repeatN n a = a >> repeatN (n-1) a

for :: [a] -> (a -> IO ()) -> IO ()
for [] fa = return ()
for (x:xs) fa = fa x >> for xs fa

sequence' :: [IO a] -> IO [a] -- we use the name sequence' since sequence is predefined
sequence' [] = return []
sequence' (a:as) = 
  do
    r <- a
    rs <- sequence' as
    return (r:rs)

{- Instead of having a fixed collection of control structures as in most other languages, we
can instead invent new ones, possibly application specific, as the need arises. That's
a quite powerful technique! -}
    
{- 
Monad Laws.
------------

All monads should obey the following properties.

    "Left identity": return a >>= f  =  f a
    "Right identity":  m >>= return  =  m
    "Associativity":  (m >>= f) >>= g  =  m >>= (\x -> f x >>= g)

Why should these laws hold? See http://www.haskell.org/haskellwiki/Monad_Laws


Generic monad operations.
-------------------------
The monad interface is powerful enough to generalize many well-known standard functions, e.g., for lists.

Here are some examples:

fmap :: Monad m => (a -> b) -> m a -> m b
fmap f m = do x <- m; return (f x)

Note: In Haskell, the typeclass constraint of fmap is "Functor m" instead of "Monad m".

join :: Monad m => m (m a) -> m a

foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a

mapM :: Monad m => (a -> m b) -> [a] -> m [b]

Exercise: What is the implementation of these functions? What do these functions do in each of the
monads you know?
-}
    
    
{- Let us now return to the problem of modularizing our interpreters.

In the BCFAE language, function composition is particularly sophisticated, because both an environment
and a store have to be passed around at the same time, each with its own dataflow. We have already
seen how to do each of those in isolation, namely in the form of the Reader monad and the State monad,
respectively.

We will first show how to capture this form of function composition in a single monad we call "StateReader";
later we will discuss how we can compose this monad from simpler monads such as the state and reader monad.

In BCFAE, we have to pass both an environment r and a state s. The environment is just pushed into the
computation; the state is pushed in but the computation also produces a new state. This justifies the following
type definition:
-}
newtype StateReader r s a = StateReader {
    runStateReader :: r -> s -> (a,s)
}

{- The unit computation in the StateReader monad ignores its environment and returns the state unchanged,
hence the "return" computation for a value a is \_ s -> (a,s). The bind operator in this monad captures
the dataflow pattern for environments and states: Push the environment into the subcomputation; thread
the state through the computation.

Note: When reading these definitions it is useful to ignore the wrapping/unwrapping operations (StateReader,
runStateReader), since they are just an artefact of Haskell. 
-}

instance Monad (StateReader r s) where
    return a = StateReader $ \_ s -> (a,s)
    m >>= k  = StateReader $ \r s -> let (a,s') = (runStateReader m r s) in (runStateReader (k a)) r s'


{- For StateReader, we can now define variants of ask and local again. -}    
askR' :: StateReader a s a
askR' = StateReader (\r s -> (r,s))

localR' :: (r -> b) -> StateReader b s a -> StateReader r s a
localR' f m = StateReader $ (\r s -> (runStateReader m) (f r) s)

{- For accessing the state, it is useful to have an operation to get the current state, and an operation to
update the current state. -}    
getS :: StateReader r s s 
getS = StateReader (\_ s -> (s,s))

putS :: s -> StateReader r s ()
putS s = StateReader (\r _ -> ((),s))

{- Now let's implement BCFAE using the StateReader monad. Here is the definition of abstract syntax and runtime entities. -}

data Exp5 = Num5 Int | Id5 String | Add5 Exp5 Exp5 | Fun5 String Exp5 | App5 Exp5 Exp5 
            | NewBox5 Exp5 | OpenBox5 Exp5 | SetBox5 Exp5 Exp5 | Seq5 Exp5 Exp5 | If0 Exp5 Exp5 Exp5 deriving (Show,Eq)
wth5 x e body = App5 (Fun5 x body) e

data Value5 = NumV5 Int | ClosureV5 String Exp5 Env5 | Address5 Int deriving (Show,Eq)
type Env5 = Map String Value5
type Store5 = (Map Int Value5,Int)

{- The interpreter parameterizes the StateReader monad with its choice for representing environments and states: Env5 and Store5. 
Notice (again) that only those branches of the interpreter that do something with states or environments are coupled to
their existence; the other branches use only the generic monad operations. -}

eval5 :: Exp5 -> StateReader Env5 Store5 Value5
eval5 (Num5 n) = return $ NumV5 n 
eval5 (Add5 l r) = 
 do (NumV5 v1) <- eval5 l
    (NumV5 v2) <- eval5 r
    return (NumV5 (v1+v2))
eval5 (Id5 x) = 
  do env <- askR'
     return (env ! x)
eval5 (Fun5 param body) = 
  do
    env <- askR'
    return $ ClosureV5 param body env
eval5  (App5 f a) = 
  do
   (ClosureV5 param body cenv) <- eval5 f
   av <- eval5 a
   localR' (\env -> (insert param av cenv)) (eval5 body)
eval5 (If0 e1 e2 e3) =
  do
    (NumV5 n) <- eval5 e1
    if (n == 0) then eval5 e2 else eval5 e3    
eval5 (NewBox5 e) = 
  do
    ev <- eval5 e
    (s,nextFree) <- getS
    putS (insert nextFree ev s, nextFree+1)
    return $ Address5 nextFree
eval5 (OpenBox5 e) = 
  do
    (Address5 i) <- eval5 e
    (s,_) <- getS
    return $ s ! i    
eval5 (SetBox5 e1 e2) = 
  do
    (Address5 i) <- eval5 e1
    e2v <- eval5 e2
    (s,nfa) <- getS
    putS (insert i e2v s, nfa)
    return e2v
eval5 (Seq5 e1 e2) = 
  do
    eval5 e1
    eval5 e2    

{- The example from 9-bcfae.scala -}  
test5 = wth5 "switch"  (NewBox5 (Num5 0))
             (wth5 "toggle"  (Fun5 "dummy" (If0 (OpenBox5 (Id5 "switch"))
                                              (Seq5 (SetBox5 (Id5 "switch") (Num5 1)) (Num5 1))
                                              (Seq5 (SetBox5 (Id5 "switch") (Num5 0)) (Num5 0))))
               (Add5 (App5 (Id5 "toggle") (Num5 42)) (App5 (Id5 "toggle") (Num5 42))))

main5 :: Exp5 -> (Value5, Store5)
main5 e = runStateReader (eval5 e) empty (empty,0)




{-
Monad Transformers
==================
Obviously our StateReader monad is in some way a composition of the State monad and the Reader monad.
This begs the question whether we can also compose the State and the Reader monad.

Obviously we could use, say, the state monad produce values in the reader monad, but that is
in general not the desired semantics, because the bind and return operator would only go
through one monad. For instance, "return 42" would not yield \s -> \env -> (42,s).

Hence we need a deeper kind of integration of the monads. This is unfortunately not possible with the
existing definitions of the monads. What we can do instead is to write _monad transformers_.
For many monads, we can write a version of this monad that takes another monad as parameter.
This does not work for every monad (e.g., not for the list monad), and the transformer version
can also not be derived in an automatic way.

Let us illustrate the idea with the Reader monad. This is what the reader monad transformer looks like:
-}

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Monad m) => Monad (ReaderT r m) where
    return a = ReaderT $ \_ -> return a -- The "return" in the body refers to the return function of m
    m >>= k  = ReaderT $ \r -> do       -- The do notation in the body refers to m, not ReaderT r m
        a <- runReaderT m r             -- hence these definitions are not recursive!
        runReaderT (k a) r
  
askT :: Monad m => ReaderT r m r
askT       = ReaderT return

localT f m = ReaderT $ \r -> runReaderT m (f r)  

{- Monad transformers enable us to compose monads in a chain of transformers terminated with an ordinary monad.
To navigate within such a chain, it makes sense to have a "lift" function, with which we can lift values
from the inner monad to the outer monad. -}

lift :: Monad m => m a -> ReaderT r m a 
lift m = ReaderT $ \_ -> m

{- Using lift, we can define variants of the inner monad functions for the composed monad: -}
getT = lift getState
putT = lift . putState

{- Here is eval5 written using the composed monad: -}
eval5' :: Exp5 -> ReaderT Env5 (State Store5) Value5
eval5' (Num5 n) = return $ NumV5 n 
eval5' (Add5 l r) = 
 do (NumV5 v1) <- eval5' l
    (NumV5 v2) <- eval5' r
    return (NumV5 (v1+v2))
eval5' (Id5 x) = 
  do env <- askT
     return (env ! x)
eval5' (Fun5 param body) = 
  do
    env <- askT
    return $ ClosureV5 param body env
eval5'  (App5 f a) = 
  do
   (ClosureV5 param body cenv) <- eval5' f
   av <- eval5' a
   localT (\env -> (insert param av cenv)) (eval5' body)
eval5' (If0 e1 e2 e3) =
  do
    (NumV5 n) <- eval5' e1
    if (n == 0) then eval5' e2 else eval5' e3    
eval5' (NewBox5 e) = 
  do
    ev <- eval5' e
    (s,nextFree) <- getT
    putT (insert nextFree ev s, nextFree+1)
    return $ Address5 nextFree
eval5' (OpenBox5 e) = 
  do
    (Address5 i) <- eval5' e
    (s,_) <- getT
    return $ s ! i    
eval5' (SetBox5 e1 e2) = 
  do
    (Address5 i) <- eval5' e1
    e2v <- eval5' e2
    (s,nfa) <- getT
    putT (insert i e2v s, nfa)
    return e2v
eval5' (Seq5 e1 e2) = 
  do
    eval5' e1
    eval5' e2   

main5' :: Exp5 -> (Value5, Store5)
main5' e = runState (runReaderT (eval5' e) empty) (empty,0)


{- 
Monads and Continuations - the Continuation Monad.    
==================================================

Monadic style is similar to continuation passing style. This similarity can
be formalized in two ways: 1) We can define a _continuation monad_, whose
effect is to CPS-transform a program in monadic style. 2) By choosing
a suitable space of answers, a CPS interpreter can act as a monad interpreter.

Here we will only elaborate on possibility 1). For 2), we refer to Sec. 3.3
of the Wadler paper cited in the beginning of this document.

Here is the definition of the continuation monad.
-}
    
newtype Cont a b = Cont { runCont :: (b -> a) -> a }
 
instance Monad (Cont a) where 
   m >>= f = Cont (\c -> (runCont m) (\b -> (runCont (f b)) c))
   return x = Cont (\c -> c x)
    
{- Using the continuation monad, we can even define a variant of Racket's letcc.
In contrast to Racket, it only works in code that has been lifted to the continuation
monad, though. -}
    
callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k

-- Example from 15-forkjoin.rkt
-- (define (f n) (+ 10 (* 5 (let/cc k (/ 1 (if (zero? n) (k 1) n))))))

f :: Float -> Cont a Float
f n = do
       a <- callCC (\k -> do b <- (if (n == 0) then (k 1) else (return n))
                             return (1 / b))
       return $ 10 + 5*a

testf n = runCont (f n) id        

{- Using the continuation monad, we can now write our interpreter for FAE with letcc
more elegantly. Since we also need environments, we use the reader monad transformer
to compose reader and continuation monad.

Exercise: Would it also make sense to define a continuation monad transformer and
compose the two monads the other way around? Does it make a difference? -}
                                             
data Exp6 = Num6 Int | Id6 String | Add6 Exp6 Exp6 | Fun6 String Exp6 
          | App6 Exp6 Exp6 | Letcc6 String Exp6 deriving (Show,Eq)
data Value6 = NumV6 Int | ClosureV6 String Exp6 Env6 
            | ContV6 (Value6  -> Value6) 
type Env6 = Map String Value6

eval6 :: Exp6 -> ReaderT Env6 (Cont Value6) Value6 
eval6 (Num6 n) = return $ NumV6 n 
eval6 (Add6 l r) = 
 do (NumV6 v1) <- eval6 l
    (NumV6 v2) <- eval6 r
    return (NumV6 (v1+v2))
eval6 (Id6 x) = 
  do env <- askT
     return (env ! x)
eval6 (Fun6 param body) = 
  do
    env <- askT
    return $ ClosureV6 param body env
eval6  (App6 f a) = 
  do
   fv <- eval6 f
   case fv of 
     (ClosureV6 param body cenv) -> 
       (do  av <- eval6 a
            localT (\env -> (insert param av cenv)) (eval6 body))
     (ContV6 f) ->
       (do av <- eval6 a
           ReaderT $ \_ ->
             Cont $ \k -> (f av)) -- ignore k       
eval6 (Letcc6 c body) =
  ReaderT $ \env -> 
    Cont $ \k ->
     runCont (runReaderT (eval6 body) (insert c (ContV6 k) env)) k
     
test6 = App6 (Fun6 "x" (Add6 (Id6 "x") (Num6 5))) (Num6 7)
runtest6 = let (NumV6 n) = (runCont (runReaderT (eval6 test6) empty)) id in n

test6' = Add6 (Num6 1) (Letcc6 "k" (Add6 (Num6 2) (App6 (Id6 "k") (Num6 3)))) 
runtest6' = let (NumV6 n) = (runCont (runReaderT (eval6 test6') empty)) id in n
