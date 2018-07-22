{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Scrabble where

import Data.Char

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score s) = s

instance Monoid Score where
    mempty = Score 0

instance Semigroup Score where
    (<>) = (+)

score :: Char -> Score
score c
  | upperC `elem` "AEILNORSTU" = Score 1
  | upperC `elem` "DG" = Score 2
  | upperC `elem` "BCMP" = Score 3
  | upperC `elem` "FHVWY" = Score 4
  | upperC `elem` "K" = Score 5
  | upperC `elem` "JX" = Score 8
  | upperC `elem` "QZ" = Score 10
  | otherwise = Score 0
  where upperC = toUpper c


scoreString :: String -> Score
scoreString = mconcat . map score
