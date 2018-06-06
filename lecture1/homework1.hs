{-# OPTIONS_GHC -Wall #-}

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = mod n 10 : toDigitsRev (div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x:y:zs) = x : 2*y : doubleEveryOtherRev zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits xs = sum (map (sum . toDigits) xs)

validate :: Integer -> Bool 
validate n = reminder == 0
    where reminder = (sumDigits . doubleEveryOther . toDigits) n `mod` 10

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n p1 p2 p3
  | n <= 0 = []
  | n == 1 = [(p1, p2)]
  | otherwise = step1 ++ step2 ++ step3
  where step1 = hanoi (n-1) p1 p3 p2
        step2 = [(p1, p2)]
        step3 = hanoi (n-1) p3 p1 p2
