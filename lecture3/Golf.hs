module Golf where
import Data.List

skips :: [a] -> [[a]]
skips xs = map everyN [1..(length xs)]
    where everyN n = map snd $ filter (\x -> fst x `mod` n == 0) $ zip [1..] xs

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map snd' (filter isBiggerThanNeighbors triples)
    where triples = zip3 xs (drop 1 xs) (drop 2 xs)
          isBiggerThanNeighbors (i, j, k) = j > i && j > k
          snd' (i, j, k) = j

frequency :: [Integer] -> Integer -> Int
frequency xs x = length $ filter (x==) xs

count :: [Integer] -> [Int]
count xs = map (frequency xs) [1..9]

draw :: Int -> Int -> String
draw maxFreq n = replicate (maxFreq - n) ' ' ++ replicate n '*'

histogram :: [Integer] -> String
histogram xs = 
    let maxFreq = maximum $ map (frequency xs) [1..9] 
     in unlines ((transpose $ map (draw maxFreq) (count xs)) ++ [replicate 9 '=', "123456789"])
