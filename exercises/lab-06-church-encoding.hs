{-# LANGUAGE RankNTypes #-}

import Prelude ((+), ($), Integer(..))

type Nat = forall a. (a -> a) -> a -> a

zero :: Nat
zero = \s z -> z

succ :: Nat -> Nat
succ n = \s z -> s (n s z)

one = succ zero
two = succ one
three = succ two
four = succ three
five = succ four

add :: Nat -> Nat -> Nat
add x y = \s z -> x s (y s z)

mult :: Nat -> Nat -> Nat
mult x y = \s z -> x (y s) z

exp :: Nat -> Nat -> Nat
exp x y = y x

print :: Nat -> Integer
print x = x (+1) 0
