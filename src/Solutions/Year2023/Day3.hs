{-# LANGUAGE TupleSections #-}
module Solutions.Year2023.Day3 where

import qualified Data.Map as M
import Data.Maybe (mapMaybe, isJust, catMaybes)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared

data Token = Number Int | Symbol Char
data ParserToken = Token Token | Linebreak | Dots Int

isNumber (Number _) = True
isNumber _ = False

positionMap = M.fromList . third . foldl helper (0, 0, [])
  where helper (rpos, cpos, xs) x = case x of
                                      Token t@(Number n) -> (rpos, cpos + length (show n), ((rpos, cpos), t):xs)
                                      Token t -> (rpos, cpos + 1, ((rpos, cpos), t):xs)
                                      Dots n -> (rpos, cpos + n, xs)
                                      Linebreak -> (rpos + 1, 0, xs)
        third (_, _, x) = x

neighborMap m  = map helper . filter (isNumber . snd) . M.toAscList $ m
  where helper ((row, col), Number n) = (n, neighborTokens)
          where neighborTokens = mapMaybe checkToken $ neighborCoords row col (length . show $ n)
                checkToken p = case m M.!? p of
                                 Nothing -> Nothing
                                 Just (Number _) -> Nothing
                                 Just (Symbol c) -> Just (p, c)
                neighborCoords row col len = left:right:(above ++ below)
                  where above = map (row - 1, ) [col-1..col+len]
                        below = map (row + 1, ) [col-1..col+len]
                        left = (row, col - 1)
                        right = (row, col + len)

parser = neighborMap . positionMap <$> many token
  where token = intToken <|> dots <|> lineBreak <|> symbolToken
        lineBreak = Linebreak <$ newline
        dots = Dots . length <$> some (char '.')
        intToken = Token . Number <$> decimal
        symbolToken = Token . Symbol <$> asciiChar

reverseMap = foldr helper M.empty
  where helper (n, ts) m = foldr addNumber m . filter ((== '*') . snd) $ ts
          where addNumber (posc, _) = M.insertWith (++) posc [n]

solve1 = foldl (\acc (n, ss) -> acc + if not (null ss) then n else 0) 0
solve2 = foldl (\acc ns -> acc + if length ns == 2 then product ns else 0) 0 . reverseMap

solution = runSolution parser solve1 solve2
