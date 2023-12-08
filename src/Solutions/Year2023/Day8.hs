module Solutions.Year2023.Day8 where

import qualified Data.Map.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char

import Shared

parser = (,) <$> directions <* count 2 newline <*> links
  where directions = many (fst <$ char 'L' <|> snd <$ char 'R')
        links = M.fromList <$> sepBy link newline
        link = (,) <$> location <* string " = (" <*> ((,) <$> location <* string ", " <*> location) <* char ')'
        location = count 3 (upperChar <|> digitChar) -- digitChar only needed for demo data

solve path m isEnd start = step (cycle path) m start 0
  where step (p:ps) m loc n
          | isEnd loc = n
          | otherwise = step ps m (p (m M.! loc)) (n + 1)

solve1 (path, m) = solve path m (== "ZZZ") "AAA" 

-- in the given data, paths run a cycle and there's only one end point in the cycle (and none before), which
-- happens to be `cycleLength` steps from the start. This makes this pretty easy (see history for more general code):
solve2 (path, m) = foldl1 lcm . map (solve path m ((== 'Z') . last)) . filter ((== 'A') . last) . M.keys $ m

solution = runSolution parser solve1 solve2
