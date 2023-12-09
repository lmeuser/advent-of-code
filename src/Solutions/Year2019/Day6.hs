module Solutions.Year2019.Day6 where

import qualified Data.Map as M
import Data.Tuple (swap)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared

parser = sepBy orbit newline
  where orbit = (,) <$> obj <* char ')' <*> obj
        obj = some alphaNumChar

solve1 inp = sum . M.elems . step [("COM", 0)] $ M.empty
  where m = M.fromListWith (++) . map (\(a, b) -> (a, [b])) $ inp
        step [] dm = dm
        step ((x, v):xs) dm = let dm' = M.insert x v dm
                              in case m M.!? x of
                                   Just next -> step ([(i, v + 1) | i <- next] ++ xs) dm'
                                   Nothing -> step xs dm'

solve2 inp = minimum . M.elems . M.intersectionWith (+) (distmap "YOU") $ distmap "SAN"
  where m = M.fromList . map swap $ inp
        distmap s = step (m M.! s) 0 M.empty
          where step "COM" _ dm = dm
                step s d dm = step (m M.! s) (d + 1) (M.insert s d dm)

solution = runSolution parser solve1 solve2
