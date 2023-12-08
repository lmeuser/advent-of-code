module Solutions.Year2023.Day8 where

import Data.List (elemIndex, union)
import qualified Data.Map.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char

import Shared
import qualified Data.Bits as lengths

parser = (,) <$> directions <* count 2 newline <*> links
  where directions = many (fst <$ char 'L' <|> snd <$ char 'R')
        links = M.fromList <$> sepBy link newline
        link = (,) <$> location <* string " = (" <*> ((,) <$> location <* string ", " <*> location) <* char ')'
        location = count 3 (upperChar <|> digitChar)

solve1 (path, m) = step (cycle path) m "AAA" 0
  where step _ _ "ZZZ" n = n
        step (p:ps) m loc n = step ps m (p (m M.! loc)) (n + 1)

-- this finds cycles and returns an info tuple consisting of:
--   - cycle length
--   - cycle starting point
--   - points in cycle that are end points (end with Z)
-- So in theory, the end points for a given path are:
--   info[1] + (any entry of info[2]) + n * info[0] (for any int n)
-- In practice, turns out every cycle contains exactly one end point, and
-- that always occurs `cyclelength` steps after the start (i.e. info[1] + info[2] = info[0])
-- Therefore, the formula above simplies to n * cyclelength (for any n ≥ 1),
-- meaning we only need to find the lowest common multiple of all the cycle lengths.
-- This also assumes that the solution is only found in the cycle and not before (which is the case for the given data).

-- solve2 (path, m) = product . foldl1 union . map (prime_factors . fst3 . findCycle) $ starts
--   where starts = filter ((== 'A') . last) (M.keys m)
--         fst3 (x, _, _) = x
--         findCycle start = helper start path' 0 []
--           where helper loc (p:ps) n xs = let entry = (loc, n `mod` length path)
--                                              nextLoc = p (m M.! loc)
--                                          in case entry `elemIndex` xs of
--                                               Just n -> let cycleLen = n + 1
--                                                             cycleStart = length xs - cycleLen
--                                                             endPoints = map fst . filter ((== 'Z') . last . fst . snd) . zip [n, n - 1..0] $ xs
--                                                         in (cycleLen, cycleStart, endPoints)
--                                               Nothing -> helper nextLoc ps (n + 1) (entry:xs)
--         path' = cycle . map (\x -> if x == 'L' then fst else snd) $ path

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
