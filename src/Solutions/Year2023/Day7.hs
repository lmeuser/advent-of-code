module Solutions.Year2023.Day7 where

import Data.Char (digitToInt)
import Data.List (group, sort, sortBy, sortOn)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared

parser = sepBy line newline
  where line = (,) <$> hand <* char ' ' <*> decimal
        hand = count 5 card
        card = (14 <$ char 'A') <|> (13 <$ char 'K') <|> (12 <$ char 'Q') <|> (11 <$ char 'J') <|> (10 <$ char 'T') <|> (digitToInt <$> digitChar)

handValue = sortBy (flip compare) . map length . group . sort

solve vf hf = sum . zipWith (*) [1..] . map snd . sortOn (\(h, _) -> (vf h, hf h))

solve1 = solve handValue id

solve2 = solve handValue' revalueJokers
  where revalueJokers = map (\c -> if c == 11 then 1 else c)
        handValue' = addJokers . handValue . filter (/= 11)
        addJokers [] = [5]
        addJokers xs@(x:xr) = (x + 5 - sum xs):xr

solution = runSolution parser solve1 solve2
