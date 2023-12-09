module Solutions.Year2019.Day4 where

import Data.List (group)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared


parser = (,) <$> decimal <* char '-' <*> decimal

valid f n = hasDouble && noDecrease
  where hasDouble = any (f . length) (group ds)
        noDecrease = all (uncurry (<=)) (zip ds (tail ds))
        ds = reverse (digits n)
        digits 0 = []
        digits n = let (r, d) = n `divMod` 10 in d:digits r

solve f (a, b) = length . filter id . map (valid f) $ [a..b]

solution = runSolution parser (solve (>=2)) (solve (==2))
