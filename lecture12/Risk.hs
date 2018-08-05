{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- Exercice 1

threeInts :: Rand StdGen (Int, Int, Int)
threeInts =getRandom >>= \i1 ->getRandom >>= \i2 ->getRandom >>= \i3 ->return (i1,i2,i3)

testDie :: DieValue
testDie = evalRand die (mkStdGen 100)

-- Exercice 2
rSort :: (Ord a) => [a] -> [a]
rSort = sortBy (flip compare)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield nAttackers nDefenders) = do
    xs <- replicateM (max 3 nAttackers) die
    ys <- replicateM (max 2 nDefenders) die
    let zs = zip (rSort xs) (rSort ys)
        (wins, loses) = partition (uncurry (>)) zs
        dAttackers = length loses
        dDefenders = length wins
    return (Battlefield (nAttackers - dAttackers) (nDefenders - dDefenders))

-- Exercice 3
invade :: Battlefield -> Rand StdGen Battlefield
invade bf
  | attackers bf <= 2 || defenders bf <= 0 = return bf
  | otherwise = battle bf >>= invade

-- Exercice 4
successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
    xs <- replicateM 1000 (invade bf)
    let nSuccess = sum $ map (\x -> if defenders x <= 0 then 1.0 else 0.0) xs
    return (nSuccess / 1000)
