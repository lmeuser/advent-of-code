module Solutions.Year2022.Day15 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Shared (runSolution, Parser)
import Data.Maybe (fromJust)
import Data.List (sort, group)

parser = sepBy sensor newline
  where sensor = (,) <$> (string "Sensor at " *> point) <*> (string ": closest beacon is at " *> point)
        point = (,) <$> (string "x=" *> number <* string ", y=") <*> number
        number = signed hspace decimal

dist (a, b) (c, d) = abs (a - c) + abs (b - d)

rangesForY y = mergeRanges . sort . map fromJust . filter (/= Nothing) . map (uncurry range)
  where range s@(sx, sy) b@(bx, by) = let d = dist s b - abs (sy - y)
                                      in if d >= 0
                                         then Just (sx - d, sx + d)
                                         else Nothing
        mergeRanges ((as, ae):(bs, be):rest) = if ae >= bs - 1 then mergeRanges ((as, max ae be):rest) else (as, ae):mergeRanges ((bs, be):rest)
        mergeRanges l = l

solve1 inp = (+ (-length beacons)) . sum . map (\(a, b) -> b - a + 1) . rangesForY y $ inp
  where beacons = group . sort . filter ((== y) . snd) . map snd $ inp
        y = 2000000

-- super slow and inefficient brute force...
solve2 inp = let [(y, x)] = [(y, (+1) . snd . head $ r) | y <- [0..4000000], let r = rangesForY y inp, length r > 1]
             in x * 4000000 + y

solution = runSolution parser solve1 solve2