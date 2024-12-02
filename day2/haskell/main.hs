-- First check if it is increasing, if it is then reverse
decreasingReport :: [Int] -> [Int]
decreasingReport x =
  if head x < head (tail x)
    then reverse x
    else x

isSafe :: Int -> Int -> Bool
isSafe x y = (x - y) >= 1 && (x - y) <= 3

checkSafety x =
  let pairs = zip x (tail x)
  map (\(x, y) -> isSafe x y) pairs

-- readNumbers :: FilePath -> IO [[Int]]
readNumbers filepath = do
  contents <- readFile filepath
  let levels = map parseLine (lines contents)
  let decreasingLevels = (map decreasingReport levels)
  return map checkSafety decreasingLevels

parseLine :: String -> [Int]
parseLine line =
  map read (words line)

main :: IO ()
main = do
  nums <- readNumbers "../test.txt"
  print nums
