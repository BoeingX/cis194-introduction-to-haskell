{-# OPTIONS_GHC -Wall #-}
import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> if even x then x - 2 else 1)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . map (\x -> if even x then x else 0) . takeWhile (/= 1) . iterate f
    where f n
            | even n = n `div` 2
            | otherwise = 3 * n + 1

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

-- |The following code has complexity O(2^n)
-- A possible improvement is to modify the definition of Tree
-- which records not only the height but also the total number of nodes of the tree
-- in this way we can achieve O(n) complexity
foldTree :: [a] -> Tree a
foldTree = foldr acc Leaf
    where acc x Leaf = Node 0 Leaf x Leaf
          acc x (Node _ lTree n rTree)
            | heightChooseLeft < heightChooseRight = Node heightChooseLeft lTree' n rTree
            | otherwise = Node heightChooseRight lTree n rTree'
            where lTree' = acc x lTree
                  rTree' = acc x rTree
                  (lHeight, rHeight) = (height lTree, height rTree)
                  (lHeight', rHeight') = (height lTree', height rTree')
                  heightChooseLeft = 1 + max lHeight' rHeight
                  heightChooseRight = 1 + max lHeight rHeight'
          height Leaf = -1
          height (Node h _ _ _) = h

xor :: [Bool] -> Bool
xor = foldr (\x acc -> if x then not acc else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ [1..n] \\ toFilter
    where toFilter = [i + j + 2 * i * j | i <- [1..n], j <- [1..n], i + j + 2 * i * j <= n]
