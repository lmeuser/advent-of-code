module Solutions.Year2021.Day13 where

import Data.List (intercalate, transpose)
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Shared (runSolution, Parser, DisplayString (DisplayString))


data Fold = X Int | Y Int
  deriving Show

parser :: Parser (S.Set (Int, Int), [Fold])
parser = (,) <$> coordSection <* space <*> foldSection
  where coordSection = S.fromList <$> endBy ((,) <$> decimal <* char ',' <*> decimal) newline
        foldSection = sepBy foldLine newline
        foldLine = makeFold <$> (string "fold along " *> (char 'x' <|> char 'y') <* char '=') <*> decimal
        makeFold 'x' = X
        makeFold 'y' = Y


applyFold (X n) (x, y) = (n - abs (x - n), y)
applyFold (Y n) (x, y) = (x, n - abs (y - n))

solve1 (set, f:_) = S.size (S.map (applyFold f) set)

foldAll (set, folds) = foldl (\s f -> S.map (applyFold f) s) set folds

displayGrid set = intercalate "\n" [[if (x, y) `S.member` set then '█' else ' ' | x <- [0..maxX]] | y <- [0..maxY]]
  where maxX = S.findMax . S.map fst $ set
        maxY = S.findMax . S.map snd $ set

solve2 = DisplayString . displayGrid . foldAll

solution = runSolution parser solve1 solve2
