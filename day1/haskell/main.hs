module NumberParser where

import Data.List (sort)

-- | Reads two numbers per line and separates them into two lists
readNumberPairs :: FilePath -> IO ([Int], [Int])
readNumberPairs filepath = do
    contents <- readFile filepath
    let pairs = map parseLine (lines contents)
    let (firsts, seconds) = unzip pairs
    return (sort firsts, sort seconds)

-- | Parse a line containing two space-separated numbers
parseLine :: String -> (Int, Int)
parseLine line = 
    let [n1, n2] = map read (words line)
    in (n1, n2)

sumDiffs :: [Int] -> [Int] -> Int
sumDiffs x1 x2 = sum $ map abs $ zipWith (-) x1 x2

similarityScore :: [Int] -> [Int] -> Int
similarityScore x1 x2 = sum similarity where
  count x = length $ filter (==x) x2
  similarity = map (\a -> a * count a) x1

-- | Example usage function
main :: IO ()
main = do
    (list1, list2) <- readNumberPairs "input"
    print $ sumDiffs list1 list2
    print $ similarityScore list1 list2


