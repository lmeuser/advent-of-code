module Days.Day4 where
import Shared (runSolution)

parseLine :: String -> [Integer]
parseLine = map read . numbers [] []
  where numbers r c [] = r ++ [c]
        numbers r c (x:xs)
          | x `elem` "-," = numbers (r ++ [c]) [] xs
          | otherwise     = numbers r (c ++ [x]) xs


solution1 = toInteger . length . filter (\[a, b, c, d] -> (a >= c && b <= d) || (a <= c && b >= d))
solution2 = toInteger . length . filter (\[a, b, c, d] -> c <= b && a <= d)


solution = runSolution (solution1 . pairs) (solution2 . pairs)
  where pairs = map parseLine . lines
