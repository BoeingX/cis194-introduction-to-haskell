{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
module JoinList where

import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

-- Exercice 1
tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2


-- Exercice 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n _
  | n < 0 = Nothing
indexJ n (Single _ a)
  | n == 0 = Just a
  | otherwise = Nothing
indexJ n (Append m jl1 jl2)
  | sizeN >= sizeM = Nothing
  | sizeN < size1 = indexJ n jl1
  | otherwise = indexJ (n - getSize size1) jl2
    where sizeN = Size n
          sizeM = size m
          size1 = size $ tag jl1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl
  | n <= 0 = jl
dropJ _ (Single _ _) = Empty
dropJ n (Append m jl1 jl2)
  | sizeN >= sizeM = Empty
  | sizeN < size1 = dropJ n jl1 +++ jl2
  | otherwise = dropJ (n - getSize size1) jl2
    where sizeN = Size n
          sizeM = size m
          size1 = size $ tag jl1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n _
  | n <= 0 = Empty
takeJ _ (Single m a) = Single m a
takeJ n (Append m jl1 jl2)
  | sizeN >= sizeM = Append m jl1 jl2
  | sizeN < size1 = takeJ n jl1 +++ jl2
  | otherwise = jl1 +++ takeJ (n - getSize size1) jl2
    where sizeN = Size n
          sizeM = size m
          size1 = size $ tag jl1

-- Exercice 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercice 4
getString :: JoinList (Score, Size) String -> String
getString Empty = ""
getString (Single _ s) = s
getString (Append _ jl1 jl2) = getString jl1 ++ "\n" ++ getString jl2

instance Buffer (JoinList (Score, Size) String) where
    toString = getString
    fromString = foldr ((+++) . (\x -> Single (scoreString x, Size 1) x)) Empty . lines
    line = indexJ
    replaceLine n s b = takeJ n b +++ fromString s +++ dropJ (n+1) b
    numLines = getSize . snd . tag
    value = getScore . fst . tag

initialBuffer :: (JoinList (Score, Size) String)
initialBuffer = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] 

main :: IO ()
main = runEditor editor initialBuffer
