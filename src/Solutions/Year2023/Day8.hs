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

solve1 (path, m) = step (cycle path) m "AAA" 0
  where step _ _ "ZZZ" n = n
        step (p:ps) m loc n = step ps m (p (m M.! loc)) (n + 1)

-- in the given data, end points are always at a multiple of cycle length
-- cf. earlier revision for more general version/explanation
solve2 (path, m) = foldl1 lcm . map findCycleLength $ starts
  where starts = filter ((== 'A') . last) (M.keys m)
        findCycleLength start = helper start (cycle path) 0 M.empty
          where helper loc (p:ps) n cm = let entry = (loc, n `mod` length path)
                                         in case cm M.!? entry of
                                              Just first -> n - first
                                              Nothing -> helper (p (m M.! loc)) ps (n + 1) (M.insert entry n cm)

solution = runSolution parser solve1 solve2
