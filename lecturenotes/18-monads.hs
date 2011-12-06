-- An explanations of today's lecture notes are in Sec. 1 and 2 of the following paper:
-- The essence of functional programming, Philip Wadler,  19'th Symposium on Principles of Programming Languages, ACM Press, Albuquerque, January 1992. 
-- available at http://homepages.inf.ed.ac.uk/wadler/papers/essence/essence.ps
--
-- These lecture notes are not yet finished; more explanation will be added later on.

import Control.Exception
import Data.Map 

-- The definition of Monads in the standard library is this:

-- class  Monad m  where
--     (>>=)       :: m a -> (a -> m b) -> m b
--     return      :: a -> m a
--     m >> k      =  m >>= \_ -> k


-----------------
-- Identity Monad
-----------------
data Exp1 = Num1 Int | Add1 Exp1 Exp1 deriving Show
data Value1 = NumV1 Int deriving (Show,Eq)
    
eval1 :: Monad m => Exp1 -> m Value1
eval1 (Num1 n)   = return (NumV1 n)
eval1 (Add1 l r) = 
  eval1 l >>= (\(NumV1 v1) -> 
  eval1 r >>= (\(NumV1 v2) -> 
  return (NumV1 (v1+v2))))

-- eval1' is identical to eval1 but uses do-notation   
eval1' :: Monad m => Exp1 -> m Value1
eval1' (Num1 n)   = return (NumV1 n)
eval1' (Add1 l r) = 
 do (NumV1 v1) <- eval1' l
    (NumV1 v2) <- eval1' r
    return (NumV1 (v1+v2))


newtype Identity a = Identity { runIdentity :: a }
instance Monad Identity where
   return a = Identity a
   m >>= k  = k (runIdentity m)

test1 = Add1 (Num1 3) (Num1 5)
runtest1 :: Value1
runtest1 = runIdentity (eval1 test1)

-----------------
-- Maybe Monad
-----------------

data Exp2 = Num2 Int | Add2 Exp2 Exp2 | Fail2 deriving Show
data Value2 = NumV2 Int deriving (Show,Eq)

-- The definition of the Maybe Monad in the Haskell standard library is as follows:

-- data  Maybe a  =  Nothing | Just a

-- instance  Monad Maybe  where
--    (Just x) >>= k      = k x
--    Nothing  >>= _      = Nothing
--    return              = Just
  
eval2 :: Exp2 -> Maybe Value2
eval2 (Num2 n)   = return (NumV2 n)
eval2 (Add2 l r) = 
 do (NumV2 v1) <- eval2 l
    (NumV2 v2) <- eval2 r
    return (NumV2 (v1+v2))
eval2 Fail2 = Nothing

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

