module Solutions.Year2015.Day6 where

import qualified Data.Map as Map
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
          operation = (On <$ string "turn on") <|> (Off <$ string "turn off") <|> (Toggle <$ string "toggle")
          point = (,) <$> (decimal <* char ',') <*> decimal


-- this is horribly inefficient and slow, I need to refactor this to use an actual array

allPoints :: Point -> Point -> [Point]
allPoints (minX, minY) (maxX, maxY) = [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]

applyInstructions :: (Operation -> a -> a -> a) -> (Operation -> a) -> [Instruction] -> Map.Map (Int, Int) a
applyInstructions apply dflt = foldl (\m (Instruction op start end) -> applyToPoints op m (allPoints start end)) Map.empty
    where applyToPoints op = foldl (\m' k -> Map.insertWith (apply op) k (dflt op) m')

solve1 :: [Instruction] -> Int
solve1 = Map.size . Map.filter id . applyInstructions apply dflt
    where apply On _ _ = True
          apply Off _ _ = False
          apply Toggle _ old = not old
          dflt Off = False
          dflt _ = True

solve2 :: [Instruction] -> Int
solve2 = sum . applyInstructions apply dflt
    where apply On _ old = old + 1
          apply Off _ old = max (old - 1) 0
          apply Toggle _ old = old + 2
          dflt On = 1
          dflt Off = 0
          dflt Toggle = 2


solution :: SolutionRunner
solution = runSolution parser solve1 solve2
