module Solutions.Year2022.Day13 where

import Data.List (findIndices, sort, find, elemIndex)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Shared (runSolution, Parser)
import Data.Maybe (fromJust)


data ListItem = Integer Int | List [ListItem]
  deriving (Show, Eq)

instance Ord ListItem where
  compare i@(Integer _) l@(List _) = compare (List [i]) l
  compare l@(List _) i@(Integer _) = compare l (List [i])
  compare (Integer i1) (Integer i2) = compare i1 i2
  compare (List []) (List (_:_)) = LT
  compare (List (_:_)) (List []) = GT
  compare (List []) (List []) = EQ
  compare (List (a:as)) (List (b:bs)) = case compare a b of
    EQ -> compare (List as) (List bs)
    other -> other


parser = sepBy listPair (newline <* newline)
  where listPair = (,) <$> (list <* newline) <*> list
        list = char '[' *> sepBy listItem (char ',') <* char ']'
        listItem = (Integer <$> decimal) <|> (List <$> list)

solve1 = sum . map (+1) . findIndices (uncurry (<))

solve2 inp = loc divider1 * loc divider2
  where divider1 = [List [Integer 2]]
        divider2 = [List [Integer 6]]
        sorted = sort . unpair . ((divider1, divider2):) $ inp
        unpair = concatMap (\(a, b) -> [a, b])
        loc x = (+1) . fromJust . elemIndex x $ sorted

solution = runSolution parser solve1 solve2
