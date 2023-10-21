module Days.Day16 where

import Data.List (permutations, sort)
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Shared (runSolution, Parser)

parser :: Parser (M.Map String (Int, [String]))
parser = M.fromList <$> sepBy line newline
  where line = (\a b c -> (a, (b, c))) <$> (string "Valve " *> valve) <*> (string " has flow rate=" *> decimal) <*> tunnels
        valve :: Parser String
        valve = count 2 upperChar
        tunnels = oneTunnel <|> manyTunnels
        oneTunnel = (:[]) <$> (string "; tunnel leads to valve " *> valve)
        manyTunnels = string "; tunnels lead to valves " *> sepBy valve (string ", ")

buildDistanceMap :: M.Map String (Int, [String]) -> M.Map (String, String) Int
buildDistanceMap inp = M.restrictKeys (step initial) (S.fromList [(a, b) | a <- relevantKeys, b <- relevantKeys])
  where keys = M.keys inp
        lKeys = length keys
        relevantKeys = "AA":filter ((>1) . fst . (inp !)) keys
        initial = M.fromList ([((from, to), 1) | (from, (_, tunnels)) <- M.toList inp, to <- tunnels] ++ map (\k -> ((k, k), 0)) keys)
        step dists
          | M.size dists == lKeys * lKeys = dists
          | otherwise = step . M.union dists . M.fromListWith min $ [((from, to), val) | from <- keys, through <- snd (inp ! from), to <- keys, (through, to) `M.member` dists, let val = 1 + (dists ! (through, to))]



-- solve inp = sort . map (calc 0 [] "AA") . permutations $ relevantKeys
--   where relevantKeys = filter ((>1) . fst . (inp !)) (M.keys inp)
--         dists = buildDistanceMap inp
--         calc l n _ w | l >= 30 || null w = n
--         calc l n from (to:rest) = calc (l + 1 + dists ! (from, to)) ((l, fst (inp ! to)):n) to rest

solve inp = maximum . map (calc 0 0 "AA") . permutations $ relevantKeys
  where relevantKeys = filter ((>1) . fst . (inp !)) (M.keys inp)
        dists = buildDistanceMap inp
        calc l n _ w | l >= 30 || null w = n
        calc l n from (to:rest) = calc l' n' to rest
          where l' = l + 1 + dists ! (from, to)
                n' = n + (30 - l') * fst (inp ! to)

solution = runSolution parser buildDistanceMap solve
