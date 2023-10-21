module Days.Day14 where

import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Shared (runSolution)
import Data.Maybe (isNothing, fromJust)

parser = S.fromList . concatMap process <$> sepBy line newline
  where line = sepBy point (string " -> ")
        point = (,) <$> decimal <* char ',' <*> decimal
        process ps = concatMap (uncurry pointsBetween) $ zip ps (tail ps)
        pointsBetween (x1, y1) (x2, y2)
          | x1 == x2 = [(x1, y') | y' <- [(min y1 y2)..(max y1 y2)]]
          | otherwise = [(x', y1) | x' <- [(min x1 x2)..(max x1 x2)]]

position blocked maxY hasFloor (x, y)
  | y > maxY = if hasFloor then Just (x, y) else Nothing
  -- there has to be a more elegant way to write the next three lines
  | not ((x, y + 1) `S.member` blocked) = position blocked maxY hasFloor (x, y + 1)
  | not ((x - 1, y + 1) `S.member` blocked) = position blocked maxY hasFloor (x - 1, y + 1)
  | not ((x + 1, y + 1) `S.member` blocked) = position blocked maxY hasFloor (x + 1, y + 1)
  | otherwise = Just (x, y)

addSand maxY hasFloor blocked
  | isNothing pos = blocked
  | pos == Just (500, 0) = blocked'
  | otherwise = addSand maxY hasFloor blocked'
  where pos = position blocked maxY hasFloor (500, 0)
        blocked' = S.insert (fromJust pos) blocked

solve hasFloor inp = S.size (addSand maxY hasFloor inp) - S.size inp
  where maxY = maximum . map snd . S.toList $ inp

solve1 = solve False
solve2 = solve True


solution = runSolution parser solve1 solve2
