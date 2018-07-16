{-# OPTIONS_GHC -Wall #-}
import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr str = case parsedStr of
                Nothing -> Nothing
                Just x -> Just (eval x)
              where parsedStr = parseExp Lit Add Mul str

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul
