module Solutions.Year2021.Day2 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Shared (runSolution)


data Command = Forward Int | Down Int | Up Int
  deriving Show

parser = endBy (forward <|> down <|> up) newline
  where forward = Forward <$> (string "forward " *> decimal)
        down = Down <$> (string "down " *> decimal)
        up = Up <$> (string "up " *> decimal)

applyCommand (horiz, depth) (Forward n) = (horiz + n, depth)
applyCommand (horiz, depth) (Down n)    = (horiz, depth + n)
applyCommand (horiz, depth) (Up n)      = (horiz, depth - n)

solve1 = uncurry (*) . foldl applyCommand (0, 0)

applyCommandV2 (horiz, depth, aim) (Forward n) = (horiz + n, depth + aim * n, aim)
applyCommandV2 (horiz, depth, aim) (Down n)    = (horiz, depth, aim + n)
applyCommandV2 (horiz, depth, aim) (Up   n)    = (horiz, depth, aim - n)

solve2 = (\(a, b, _) -> a * b) . foldl applyCommandV2 (0, 0, 0)

solution = runSolution parser solve1 solve2
