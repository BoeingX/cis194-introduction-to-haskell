{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.List
-- Exercice 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercie 2
fibs2 :: [Integer]
fibs2 = 0 : scanl' (+) 1 fibs2

--fibs2' :: [Integer]
--fibs2' = 0 : 1 : zipWith (+) fibs3 (tail fibs3)

-- Exercice 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons y ys) = y : streamToList ys

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-- Exercice 4
streamRepeat :: a -> Stream a
streamRepeat y = Cons y $ streamRepeat y

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons y ys) = Cons (f y) (streamMap f ys)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f y = Cons y (streamFromSeed f (f y))

-- Exercice 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons y ys) zs = Cons y (interleaveStreams zs ys)
-- Note that
-- interleaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleaveStreams xs ys))
-- is not good
-- indeed, the following definition of ruler results in an infinite loop

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)
-- or more explicitely
-- ruler = ruler' 0
--     where ruler' n = interleaveStreams (streamRepeat n) (ruler' (n+1))

-- Exercice 6 (Optional)
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate (Cons y ys) = Cons (-y) (negate ys)
    (+) (Cons y ys) (Cons z zs) = Cons (y + z) ((+) ys zs)
    (*) (Cons y ys) (Cons z zs) = Cons (y*z) (streamMap (*y) zs + ys * Cons z zs)

instance Fractional (Stream Integer) where
    (/) (Cons y ys) (Cons z zs) = q
        where q = Cons (y `div` z) (streamMap (`div` z) (ys - q * zs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x*x) 

data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
    (*) (Matrix x1 x2 x3 x4) (Matrix y1 y2 y3 y4) = Matrix z1 z2 z3 z4
        where z1 = x1*y1 + x2*y3
              z2 = x1*y2 + x2*y4
              z3 = x3*y1 + x4*y3
              z4 = x3*y2 + x4*y4

fib4 :: Integer -> Integer
fib4 n = fibn
    where Matrix fibn _ _ _ = baseMatrix ^ n
          baseMatrix = Matrix 1 1 1 0
