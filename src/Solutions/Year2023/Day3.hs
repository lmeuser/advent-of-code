{-# LANGUAGE TupleSections #-}
module Solutions.Year2023.Day3 where

import qualified Data.Map as M
import Data.Maybe (mapMaybe)
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

neighborSymbols m = filter (not . null . snd) . map helper . filter (isNumber . snd) . M.toAscList $ m
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

parser = neighborSymbols . positionMap <$> many token
  where token = intToken <|> dots <|> lineBreak <|> symbolToken
        lineBreak = Linebreak <$ newline
        dots = Dots . length <$> some (char '.')
        intToken = Token . Number <$> decimal
        symbolToken = Token . Symbol <$> asciiChar

neighborNumbers = M.elems . foldr helper M.empty
  where helper (n, ts) m = foldr (addNumber . fst) m . filter ((== '*') . snd) $ ts
          where addNumber pos = M.insertWith (++) pos [n]

solve1 = sum . map fst
solve2 = sum . map product . filter ((== 2) . length) . neighborNumbers

solution = runSolution parser solve1 solve2
