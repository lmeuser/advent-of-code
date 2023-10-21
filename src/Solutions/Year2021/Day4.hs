module Solutions.Year2021.Day4 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Shared (runSolution, Parser)
import Data.List (transpose, maximumBy, minimumBy)
import Data.Maybe (fromJust)

parser :: Parser ([Int], [[[Maybe Int]]])
parser = (,) <$> numbers <*> boards
  where numbers = sepBy decimal (char ',') <* newline <* newline
        boards = sepBy board newline
        board = endBy1 (map Just <$> (hspace *> sepBy1 decimal hspace)) newline

diag1 :: [[a]] -> [a]
diag1 = foldr (\(i, r) l -> (r !! i):l) [] . zip [0..]

diag2 :: [[a]] -> [a]
diag2 xs = foldr (\(i, r) l -> (r !! (s - i)):l) [] . zip [0..] $ xs
  where s = length xs - 1

allLines :: [[a]] -> [[a]]
allLines board = diag1 board:diag2 board:board ++ transpose board

hasBingo :: Eq a => [[Maybe a]] -> Bool
hasBingo = any (all (== Nothing)) . allLines

boardValue :: (Num a, Eq a) => [[Maybe a]] -> a
boardValue = sum . map fromJust . filter (/= Nothing) . concat


calculateBoard numbers fullBoard = step fullBoard 0 numbers
  where step board count (n:ns) = if hasBingo board'
                                  then (count, n * boardValue board')
                                  else step board' (count + 1) ns
          where board' = map (map (\x -> if x == Just n then Nothing else x)) board

solutionHelper minmax (numbers, boards) = snd . minmax helper . map (calculateBoard numbers) $ boards
  where helper (a, _) (b, _) = compare a b
  
solve1 = solutionHelper minimumBy
solve2 = solutionHelper maximumBy

solution = runSolution parser solve1 solve2
