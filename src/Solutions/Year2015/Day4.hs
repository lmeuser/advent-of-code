module Solutions.Year2015.Day4 where

import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as BS
import Text.Megaparsec
import Text.Megaparsec.Char

import Shared

parser :: Parser String
parser = many lowerChar


md5StartsWith :: BS.ByteString -> BS.ByteString -> Bool
md5StartsWith cmp input = BS.take (BS.length cmp) (encode . MD5.hash $ input) == cmp

solveFor :: String -> String -> Int
solveFor cmp input = head . filter (md5StartsWith (BS.pack cmp) . BS.pack . (input ++) . show) $ [1..]

solve1 :: String -> Int
solve1 = solveFor "00000"

solve2 :: String -> Int
solve2 = solveFor "000000"

solution :: SolutionRunner
solution = runSolution parser solve1 solve2
