module Solutions.Year2019.Intcode where

import qualified Data.IntMap.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared (Parser)

newtype Interpreter = Interpreter (M.IntMap Int)
  deriving Show

runIntcode :: Interpreter -> [Int]
runIntcode (Interpreter initialMemory) = run 0 initialMemory
  where run pos mem = case mem M.! pos of
                        1 -> add
                        2 -> multiply
                        99 -> exit
          where readMem = (mem M.!) . (mem M.!)
                writeMem target value = M.insert (mem M.! target) value mem
                add = let op1 = readMem (pos + 1)
                          op2 = readMem (pos + 2)
                          mem' = writeMem (pos + 3) (op1 + op2)
                      in run (pos + 4) mem'

                multiply = let op1 = readMem (pos + 1)
                               op2 = readMem (pos + 2)
                               mem' = writeMem (pos + 3) (op1 * op2)
                            in run (pos + 4) mem'

                exit = M.elems mem

updateMemory (Interpreter m) = Interpreter . foldr (uncurry M.insert) m

intcodeParser :: Parser Interpreter
intcodeParser = Interpreter . M.fromList . zip [0..] <$> sepBy decimal (char ',')
