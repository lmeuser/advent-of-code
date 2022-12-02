module Main where
  
import Data.List (sortBy)
import Data.List.Split (splitOn)

elves :: String -> [[Integer]]
elves = map (map read . lines) . splitOn "\n\n"

elfTotals :: String -> [Integer]
elfTotals = map sum . elves


main :: IO ()
main = do
  input <- readFile "input.txt"
  let elfData = sortBy (flip compare) . elfTotals $ input

  -- solution 1
  putStrLn . show . (!! 0) $ elfData
  
  -- solution 2
  putStrLn . show . sum . take 3 $ elfData
