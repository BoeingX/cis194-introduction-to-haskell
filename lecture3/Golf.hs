module Golf where
import Data.List

{-|
   This function returns a list where its n-th element is itself a list consisting of every nth elements of a given list.
   A helper function 'everyN' is used to return a list of every nth elements.
-}
skips :: [a] -> [[a]]
skips xs = map everyN [1..(length xs)]
    where everyN n = map snd $ filter (\x -> fst x `mod` n == 0) $ zip [1..] xs

{-|
   This function returns all the local maxima of a list.
   By definition, only those who are strictly bigger than their **two** neighbors are considered as local maxima.
   Since Haskell does not provide a convinient 'for' syntax, we could first generate triples consisting of (n-1)th, nth and (n+1)th elements and perform a 'filter' on the triples.
   Of course, we need to select only the second position of every resulting triple for the final solution.
-}
localMaxima :: [Integer] -> [Integer]
localMaxima xs = map snd' (filter isBiggerThanNeighbors triples)
    where triples = zip3 xs (drop 1 xs) (drop 2 xs)
          isBiggerThanNeighbors (i, j, k) = j > i && j > k
          snd' (i, j, k) = j

{-|
   This function returns the histogram of a list consisting of only 1 to 9.
   It first count the frequency of the nine numbers.
   Then it *draws* an intermidiate *horizontal* histogram by appending leading white spaces.
   The desired histogram is just the transpose of the *horizontal* histogram.
-}
histogram :: [Integer] -> String
histogram xs = unlines histogramVertical
    where frequency x = length $ filter (x==) xs
          count = map frequency [1..9] 
          maxFreq = maximum count
          drawBar n = replicate (maxFreq - n) ' ' ++ replicate n '*'
          histogramHorizontal = map drawBar count
          histogramVertical = transpose histogramHorizontal ++ [replicate 9 '=', "123456789"]
