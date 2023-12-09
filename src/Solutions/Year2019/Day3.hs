module Solutions.Year2019.Day3 where

import qualified Data.Map as M
import Prelude hiding (Left, Right)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared

data Direction = Up Int | Down Int | Left Int | Right Int

parser = (,) <$> wire <* newline <*> wire
  where wire = sepBy direction (char ',')
        direction = up <|> down <|> left <|> right
        up = Up <$> (char 'U' *> decimal)
        down = Down <$> (char 'D' *> decimal)
        left = Left <$> (char 'L' *> decimal)
        right = Right <$> (char 'R' *> decimal)

calcWire = (\(_, x, _) -> x) . foldl helper ((0, 0), M.empty, 0)
  where helper ((r, c), set, d) dir = let (newPos, newParts, d') = case dir of
                                                                     Up n -> ((r - n, c), zip [(i, c) | i <- [r,r-1..r-n]] [d..d+n], d+n)
                                                                     Down n -> ((r + n, c), zip [(i, c) | i <- [r..r+n]] [d..d+n], d+n)
                                                                     Left n -> ((r, c - n), zip [(r, i) | i <- [c,c-1..c-n]] [d..d+n], d+n)
                                                                     Right n -> ((r, c + n), zip [(r, i) | i <- [c..c+n]] [d..d+n], d+n)
                                          newSet = foldr (uncurry M.insert) set newParts
                                   in (newPos, newSet, d')

solve f (a, b) = minimum . map f . M.toList . M.delete (0, 0) . M.intersectionWith (+) (calcWire a) $ calcWire b

solve1 = solve (dist . fst)
  where dist (a, b) = abs a + abs b

solve2 = solve snd

solution = runSolution parser solve1 solve2
