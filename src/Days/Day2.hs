module Days.Day2 where

import Shared ( runSolution )
import Text.Megaparsec
import Text.Megaparsec.Char


data RPS = Rock | Paper | Scissors
  deriving (Show, Eq)

instance Ord RPS where
  Rock <= Scissors = False
  Paper <= Rock = False
  Scissors <= Paper = False
  _ <= _ = True

parser = endBy game newline
  where game = buildGame <$> abc <*> (char ' ' *> xyz)
        abc = satisfy (`elem` "ABC")
        xyz = satisfy (`elem` "XYZ")
        buildGame a b = (toRPS a, toRPS b)
        toRPS x | x `elem` "AX" = Rock
                | x `elem` "BY" = Paper
                | x `elem` "CZ" = Scissors

pointsFromChoice Rock = 1
pointsFromChoice Paper = 2
pointsFromChoice Scissors = 3

pointsFromResult a b = case compare a b of
  LT -> 6
  EQ -> 3
  GT -> 0

pointsFromGame (a, b) = pointsFromChoice b + pointsFromResult a b

solve1 = sum . map pointsFromGame

-- too lazy to change the parser, so R = lose, P = draw, S = win
result Rock Rock = Scissors
result Rock Scissors = Paper
result Paper x = x
result Scissors Rock = Paper
result Scissors Scissors = Rock
result x Paper = x

solve2 = solve1 . map (\(a, b) -> (a, result a b))

solution = runSolution parser solve1 solve2
