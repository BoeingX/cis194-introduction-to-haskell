{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} 
module Party where

import Employee
import Data.Tree
import Data.List

-- Exercice 1
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp : emps) (fun + empFun emp)

instance Monoid GuestList where
    mempty = GL [] 0
    
instance Semigroup GuestList where
    (<>) (GL emps1 fun1) (GL emps2 fun2) = GL (emps1 ++ emps2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2
  | gl1 > gl2 = gl1
  | otherwise = gl2

-- Exercice 2
treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold e f (Node x ts) = f x $ map (treeFold e f) ts

-- Exercice 3
nextLevel :: Employee -> [(GuestList, GuestList)]-> (GuestList, GuestList)
nextLevel emp gls = (inviteBoss, notInviteBoss)
    where notInviteBoss = mconcat $ map (uncurry moreFun) gls
          inviteBoss = glCons emp $ mconcat $ map snd gls

-- Exercice 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold mempty nextLevel

-- Exercice 5
prettyShow :: GuestList -> String
prettyShow (GL emps fun) = unlines xs
    where xs = ("Total fun: " ++ show fun): sort (map empName emps)

main :: IO ()
main = do
    contents <- getContents
    let tree = read contents :: Tree Employee
        gl = maxFun tree
    putStrLn $ prettyShow gl
