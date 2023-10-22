module Solutions.Year2015.Day6 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared

type Point = (Int, Int)

data Operation = On | Off | Toggle deriving Show

data Instruction = Instruction Operation Point Point deriving Show


parser :: Parser [Instruction]
parser = sepBy instruction newline
    where instruction = Instruction <$> operation <* char ' ' <*> point <* string " through " <*> point
          operation :: Parser Operation
          operation = (On <$ string "turn on") <|> (Off <$ string "turn off") <|> (Toggle <$ string "toggle")
          point :: Parser Point
          point = (,) <$> (decimal <* char ',') <*> decimal

solution :: SolutionRunner
solution = runSolution parser id (const 42)