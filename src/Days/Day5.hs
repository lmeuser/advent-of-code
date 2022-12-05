module Days.Day5 where

import Data.List (transpose)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

import Shared (runSolution)


parser = (,) <$> (crateSection <* labelSection) <*> moveSection
  where 
        crateSection = map (filter (/= ' ')) . transpose <$> endBy crateLine newline
        crateLine = sepBy (crate <|> emptySlot) (char ' ')
        crate = char '[' *> upperChar <* char ']'
        emptySlot = head <$> string "   "
        labelSection = many (digitChar <|> spaceChar)
        moveSection = sepBy moveLine newline
        moveLine = (,,) <$> (string "move " *> decimal) <*> (string " from " *> decimal) <*> (string " to " *> decimal)

applyMove transform (n, from, to) stacks = replace (from - 1) src' (replace (to - 1) dst' stacks)
  where (crates, src') = splitAt n (stacks !! (from - 1))
        dst' = transform crates ++ stacks !! (to - 1)
        replace i r l = let (p1, p2) = splitAt i l
                        in p1 ++ [r] ++ tail p2

calculate transform (stacks, moves) = map head $ foldl (flip (applyMove transform)) stacks moves

solve1 = calculate reverse
solve2 = calculate id

solution = runSolution parser solve1 solve2
