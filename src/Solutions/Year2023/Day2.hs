module Solutions.Year2023.Day2 where

import Data.List (find, minimumBy)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared

data Color = Red | Green | Blue deriving Eq
data Drawing = Drawing Int Int Int
data Game = Game Int [Drawing]

red (Drawing r _ _) = r
green (Drawing _ g _) = g
blue (Drawing _ _ b) = b

buildDrawing xs = Drawing red green blue
    where findColor c = maybe 0 fst . find ((== c) . snd)
          red = findColor Red xs
          green = findColor Green xs
          blue = findColor Blue xs

parser = sepBy game newline
    where game = Game <$> (string "Game " *> decimal) <* string ": " <*> sepBy drawing (string "; ")
          drawing = buildDrawing <$> sepBy singleDraw (string ", ")
          singleDraw = (,) <$> decimal <* char ' ' <*> color
          color = (Red <$ string "red") <|> (Green <$ string "green") <|> (Blue <$ string "blue")

canHappen (Game _ drawings) = all drawCanHappen drawings
    where drawCanHappen (Drawing r g b) = r <= 12 && g <= 13 && b <= 14

solve1 = sum . map getId . filter canHappen
    where getId (Game i _) = i

power (Game _ drawings) = needed red * needed green * needed blue
    where needed f = maximum (map f drawings)

solve2 = sum . map power

solution = runSolution parser solve1 solve2
