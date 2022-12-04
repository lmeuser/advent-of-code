module Days.Day1 where

import Data.List ( sortBy )
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared ( runSolution )


parser = sortBy (flip compare) . map sum <$> sepBy (endBy decimal newline) newline

solve1 = head

solve2 = sum . take 3

solution = runSolution parser solve1 solve2
