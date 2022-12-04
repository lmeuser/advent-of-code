parseLine :: String -> [Int]
parseLine = map read . numbers [] []
  where numbers r c [] = r ++ [c]
        numbers r c (x:xs)
          | x `elem` "-," = numbers (r ++ [c]) [] xs
          | otherwise     = numbers r (c ++ [x]) xs


solution1 = length . filter (\[a, b, c, d] -> (a >= c && b <= d) || (a <= c && b >= d))
solution2 = length . filter (\[a, b, c, d] -> c <= b && a <= d)

main = do
  input <- readFile "input.txt"
  let pairs = map parseLine . lines $ input
  print . solution1 $ pairs
  print . solution2 $ pairs
