import Data.Map 


-- An explanations of today's lecture notes are in Sec. 1 and 2 of the following paper:
-- The essence of functional programming, Philip Wadler,  19'th Symposium on Principles of Programming Languages, ACM Press, Albuquerque, January 1992. 
-- available at http://homepages.inf.ed.ac.uk/wadler/papers/essence/essence.ps
--
-- These lecture notes are not yet finished; more explanation will be added later on.


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

----------------
-- Reader Monad
----------------


newtype Reader r a = Reader {
    runReader :: r -> a
}

instance Monad (Reader r) where
    return a = Reader $ \_ -> a
    m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

ask :: Reader a a 
ask = Reader id

local :: (r -> b) -> Reader b a -> Reader r a
local f m = Reader $ runReader m . f

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
  do env <- ask
     return (env ! x)
eval3 (Fun3 param body) = 
  do
    env <- ask
    return $ ClosureV3 param body env
eval3  (App3 f a) = 
  do
   (ClosureV3 param body cenv) <- eval3 f
   av <- eval3 a
   local (\env -> (insert param av cenv)) (eval3 body   )

test3 = App3 (Fun3 "x" (Add3 (Id3 "x") (Num3 5))) (Num3 7)

runtest3 = (runReader $ eval3 test3) empty -- snhould be  NumV3 12

-----------------
-- List Monad
-----------------

data Exp4 = Num4 Int | Add4 Exp4 Exp4 | Amb4 Exp4 Exp4 deriving Show
data Value4 = NumV4 Int deriving (Show,Eq)

eval4 :: Exp4 -> [Value4]
eval4 (Num4 n)   = return (NumV4 n)
eval4 (Add4 l r) = 
 do (NumV4 v1) <- eval4 l
    (NumV4 v2) <- eval4 r
    return (NumV4 (v1+v2))
eval4 (Amb4 e1 e2) = eval4 e1 ++ eval4 e2


----------------------
-- StateReader Monad
----------------------

newtype StateReader r s a = StateReader {
    runStateReader :: r -> s -> (a,s)
}
instance Monad (StateReader r s) where
    return a = StateReader $ \_ s -> (a,s)
    m >>= k  = StateReader $ \r s -> let (a,s') = (runStateReader m r s) in (runStateReader (k a)) r s'

askR :: StateReader a s a
askR = StateReader (\r s -> (r,s))

localR :: (r -> b) -> StateReader b s a -> StateReader r s a
localR f m = StateReader $ (\r s -> (runStateReader m) (f r) s)
    
getS :: StateReader r s s 
getS = StateReader (\_ s -> (s,s))

putS :: s -> StateReader r s ()
putS s = StateReader (\r _ -> ((),s))


-- The BCFAE interpreter implemented using the StateReader monad

data Exp5 = Num5 Int | Id5 String | Add5 Exp5 Exp5 | Fun5 String Exp5 | App5 Exp5 Exp5 
            | NewBox5 Exp5 | OpenBox5 Exp5 | SetBox5 Exp5 Exp5 | Seq5 Exp5 Exp5 | If0 Exp5 Exp5 Exp5 deriving (Show,Eq)
wth5 x e body = App5 (Fun5 x body) e

data Value5 = NumV5 Int | ClosureV5 String Exp5 Env5 | Address5 Int deriving (Show,Eq)
type Env5 = Map String Value5
type Store5 = (Map Int Value5,Int)
malloc :: Value5 -> StateReader r Store5 Int 
malloc v = StateReader $ \r (s,nextFree) -> (nextFree,(insert nextFree v s,nextFree+1))

eval5 :: Exp5 -> StateReader Env5 Store5 Value5
eval5 (Num5 n) = return $ NumV5 n 
eval5 (Add5 l r) = 
 do (NumV5 v1) <- eval5 l
    (NumV5 v2) <- eval5 r
    return (NumV5 (v1+v2))
eval5 (Id5 x) = 
  do env <- askR
     return (env ! x)
eval5 (Fun5 param body) = 
  do
    env <- askR
    return $ ClosureV5 param body env
eval5  (App5 f a) = 
  do
   (ClosureV5 param body cenv) <- eval5 f
   av <- eval5 a
   localR (\env -> (insert param av cenv)) (eval5 body)
eval5 (If0 e1 e2 e3) =
  do
    (NumV5 n) <- eval5 e1
    if (n == 0) then eval5 e2 else eval5 e3    
eval5 (NewBox5 e) = 
  do
   ev <- eval5 e
   newAddress <- malloc ev
   return $ Address5 newAddress
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

    
test5 = wth5 "switch"  (NewBox5 (Num5 0))
             (wth5 "toggle"  (Fun5 "dummy" (If0 (OpenBox5 (Id5 "switch"))
                                              (Seq5 (SetBox5 (Id5 "switch") (Num5 1)) (Num5 1))
                                              (Seq5 (SetBox5 (Id5 "switch") (Num5 0)) (Num5 0))))
               (Add5 (App5 (Id5 "toggle") (Num5 42)) (App5 (Id5 "toggle") (Num5 42))))

main5 :: Exp5 -> (Value5, Store5)
main5 e = runStateReader (eval5 e) empty (empty,0)

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return a = State $ \s -> (a, s)
    m >>= k  = State $ \s -> let
        (a, s') = runState m s
        in runState (k a) s'

get   = State $ \s -> (s, s)
put s = State $ \_ -> ((), s)        

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Monad m) => Monad (ReaderT r m) where
    return a = ReaderT $ \_ -> return a
    m >>= k  = ReaderT $ \r -> do
        a <- runReaderT m r
        runReaderT (k a) r
  
askT       = ReaderT return
localT f m = ReaderT $ \r -> runReaderT m (f r)  

lift :: Monad m => m a -> ReaderT r m a 
lift m = ReaderT $ \_ -> m

getT = lift get
putT = lift . put

mallocT :: Value5 -> ReaderT Env5 (State Store5) Int 
mallocT v = lift $ State $ \(s,nextFree) -> (nextFree,(insert nextFree v s,nextFree+1))

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
   newAddress <- mallocT ev
   return $ Address5 newAddress
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

