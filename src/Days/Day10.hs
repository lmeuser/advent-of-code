module Days.Day10 where

import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Shared (runSolution, DisplayString(DisplayString))

data Instruction = Noop | AddX Int

parser = sepBy (noop <|> addX) newline
  where noop = const Noop <$> string "noop"
        addX = AddX <$> (string "addx " *> signed hspace decimal)

calcStates = concat . reverse . snd . foldl step (1, [])
  where step (x, l) ins = let (x', len) = case ins of
                                            Noop -> (x, 1)
                                            AddX n -> (x + n, 2)
                          in (x', replicate len x:l)

solve1 is = sum . map score $ [20, 60, 100, 140, 180, 220]
  where score n = (states !! (n - 1)) * n
        states = calcStates is

solve2 = render . map isOn . zip [0..] . calcStates
  where isOn (pos, x) = abs ((pos `mod` 40) - x) <= 1
        render = DisplayString . intercalate "\n" . chunksOf 40 . map (\b -> if b then '█' else ' ')

solution = runSolution parser solve1 solve2
