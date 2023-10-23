module Solutions.Year2015.Day6 where

import Data.Array
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Shared

type Point = (Int, Int)
data Operation = On | Off | Toggle
data Instruction = Instruction Operation Point Point

parser :: Parser [Instruction]
parser = sepBy instruction newline
    where instruction = Instruction <$> operation <* char ' ' <*> point <* string " through " <*> point
          operation = (On <$ string "turn on") <|> (Off <$ string "turn off") <|> (Toggle <$ string "toggle")
          point = (,) <$> decimal <* char ',' <*> decimal

solveWith :: (Operation -> Int -> Int) -> [Instruction] -> Int
solveWith apply = sum . accumArray (flip apply) 0 ((0, 0), (999, 999)) . (>>= expand)
    where expand (Instruction op start end) = [(index, op) | index <- range (start, end)]

apply1 :: Operation -> Int -> Int
apply1 On = const 1
apply1 Off = const 0
apply1 Toggle = (+1) . (* (-1))

apply2 :: Operation -> Int -> Int
apply2 On = (+1)
apply2 Off = max 0 . subtract 1
apply2 Toggle = (+2)

solution :: SolutionRunner
solution = runSolution parser (solveWith apply1) (solveWith apply2)
