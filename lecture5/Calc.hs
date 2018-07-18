{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
import qualified ExprT as E
import Parser
import qualified StackVM as S
import qualified Data.Map as M

-- Exercice 1
eval :: E.ExprT -> Integer
eval (E.Lit x) = x
eval (E.Add x y) = eval x + eval y
eval (E.Mul x y) = eval x * eval y

-- Exercice 2
evalStr :: String -> Maybe Integer
evalStr str = case parsedStr of
                Nothing -> Nothing
                Just x -> Just (eval x)
              where parsedStr = parseExp E.Lit E.Add E.Mul str

-- Exercice 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr E.ExprT where
    lit = E.Lit
    add = E.Add
    mul = E.Mul

-- Exercice 4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 (mod (x+y) 7)
    mul (Mod7 x) (Mod7 y) = Mod7 (mod (x*y) 7)

-- Exercice 5
instance Expr S.Program where
    lit x = [S.PushI x]
    add xs ys = xs ++ ys ++ [S.Add]
    mul xs ys = xs ++ ys ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

-- Exercice 6
class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
              | Var String
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              deriving (Show, Eq)

instance Expr VarExprT where
    lit = Lit
    add = Add
    mul = Mul

instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

-- This function could be simplified with Monad
instance Expr (M.Map String Integer -> Maybe Integer) where
    lit num _ = Just num
    add x y d = case (xd, yd) of
                  (Just m, Just n) -> Just (m + n)
                  _ -> Nothing
                where (xd, yd) = (x d, y d)
    mul x y d = case (xd, yd) of
                  (Just m, Just n) -> Just (m * n)
                  _ -> Nothing
                where (xd, yd) = (x d, y d)

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs expr = expr $ M.fromList vs
