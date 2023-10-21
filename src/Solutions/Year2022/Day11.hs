module Solutions.Year2022.Day11 where

import Data.List (sortBy)
import qualified Data.Map as M
import Data.Map ((!))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Shared (runSolution)

data Monkey = Monkey Int [Int] (Int -> Int) Int Int Int
data Operand = N Int | Old

parser = buildMap <$> sepBy monkey (newline *> newline)
  where monkey = Monkey <$> header <*> starting <*> operation <*> test <*> true <*> false
        header = string "Monkey " *> decimal <* char ':' <* newline
        starting = string "  Starting items: " *> sepBy decimal (char ',' *> char ' ') <* newline
        operation = string "  Operation: new = " *> op <* newline
        test = string "  Test: divisible by " *> decimal <* newline
        true = string "    If true: throw to monkey " *> decimal <* newline
        false = string "    If false: throw to monkey " *> decimal
        op = buildOp <$> operand <*> (string " + " <|> string " * ") <*> operand
        operand = (N <$> decimal) <|> (Old <$ string "old")
        buildOp Old operation op2 x = f x op2'
          where f = case operation of " + " -> (+)
                                      " * " -> (*)
                op2' = case op2 of Old -> x
                                   N n -> n
        buildMap ms = (M.fromList . map (\(Monkey n s _ _ _ _) -> (n, s)) $ ms, ms)

allRounds worryF (s, m) = iterate (runRound m) (s, M.empty)
  where runRound monkeys state = foldl applyMonkey state monkeys
        applyMonkey (loc, act) (Monkey n _ f test true false) = foldl step (M.insert n [] loc, act) (loc ! n)
          where step (l, a) i = (M.insertWith (++) target [i'] l, M.insertWith (+) n 1 a)
                  where i' = worryF . (`mod` baseFactor) . f $ i
                        target = if i' `mod` test == 0 then true else false
        -- multiply all the divisors, and always do worry factor modulo that result
        -- results stay the same, but the numbers don't grow too big
        baseFactor = product . map (\(Monkey _ _ _ n _ _) -> n) $ m

solve worryF round = product . take 2 . sortBy (flip compare) . map snd . M.toList . snd . (!! round) . allRounds worryF

solution = runSolution parser (solve (`div` 3) 20) (solve id 10000)
