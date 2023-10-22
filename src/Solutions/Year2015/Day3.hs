module Solutions.Year2015.Day3 where

import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char

import Shared

parser :: Parser String
parser = many (char '<' <|> char '>' <|> char '^' <|> char 'v')

setForMoves :: String -> Set.Set (Int, Int)
setForMoves = fst . foldl step (Set.fromList [(0, 0)], (0, 0))
    where step (visited, (x, y)) move = (Set.insert (x', y') visited, (x', y'))
            where (x', y') = case move of
                            '<' -> (x - 1, y)
                            '>' -> (x + 1, y)
                            '^' -> (x, y + 1)
                            'v' -> (x, y - 1)

solve1 :: String -> Int
solve1 = Set.size . setForMoves

solve2 :: String -> Int
solve2 moves = Set.size $ Set.union (setForMoves moves1) (setForMoves moves2)
    where (moves1, moves2) = split moves
          split (x:y:rest) = let (xs, ys) = split rest
                             in (x:xs, y:ys)
          split [] = ([], [])

solution :: SolutionRunner
solution = runSolution parser solve1 solve2
