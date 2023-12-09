module Solutions.Year2019.Intcode where

import qualified Data.IntMap.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared (Parser)

newtype Interpreter = Interpreter (M.IntMap Int)
  deriving Show

data AddressMode = Position | Immediate
  deriving Show

parseInstruction n = (opcode, a1, a2, a3)
  where (aDigits, opcode) = n `divMod` 100
        as n = let (r, d) = n `divMod` 10 in modeFromInt d:as r
        [a1, a2, a3] = take 3 $ as aDigits
        modeFromInt 0 = Position
        modeFromInt 1 = Immediate

runIntcode :: [Int] -> Interpreter -> ([Int], [Int])
runIntcode input (Interpreter initialMemory) = run 0 input [] initialMemory
  where run pos input output mem = case opcode of
                                     1 -> add
                                     2 -> multiply
                                     3 -> inp
                                     4 -> outp
                                     5 -> jit
                                     6 -> jif
                                     7 -> lt
                                     8 -> eq
                                     99 -> exit
                                     _ -> error ("unknown instruction " ++ show opcode)
          where (opcode, a1, a2, a3) = parseInstruction (mem M.! pos)
                readMem mode = case mode of
                                 Position -> (mem M.!) . (mem M.!)
                                 Immediate -> (mem M.!)
                writeMem mode target value = case mode of
                                               Position -> M.insert (mem M.! target) value mem
                                               Immediate -> error "writing to immediate"
                add = let op1 = readMem a1 (pos + 1)
                          op2 = readMem a2 (pos + 2)
                          mem' = writeMem a3 (pos + 3) (op1 + op2)
                      in run (pos + 4) input output mem'

                multiply = let op1 = readMem a1 (pos + 1)
                               op2 = readMem a2 (pos + 2)
                               mem' = writeMem a3 (pos + 3) (op1 * op2)
                            in run (pos + 4) input output mem'

                inp = let mem' = writeMem a1 (pos + 1) (head input)
                      in run (pos + 2) (tail input) output mem'

                outp = let op1 = readMem a1 (pos + 1)
                       in run (pos + 2) input (op1:output) mem

                jit = let op1 = readMem a1 (pos + 1)
                          op2 = readMem a2 (pos + 2)
                          pos' = if op1 /= 0 then op2 else pos + 3
                      in run pos' input output mem

                jif = let op1 = readMem a1 (pos + 1)
                          op2 = readMem a2 (pos + 2)
                          pos' = if op1 == 0 then op2 else pos + 3
                      in run pos' input output mem

                lt = let op1 = readMem a1 (pos + 1)
                         op2 = readMem a2 (pos + 2)
                         mem' = writeMem a3 (pos + 3) (if op1 < op2 then 1 else 0)
                     in run (pos + 4) input output mem'

                eq = let op1 = readMem a1 (pos + 1)
                         op2 = readMem a2 (pos + 2)
                         mem' = writeMem a3 (pos + 3) (if op1 == op2 then 1 else 0)
                     in run (pos + 4) input output mem'

                exit = (M.elems mem, reverse output)

updateMemory (Interpreter m) = Interpreter . foldr (uncurry M.insert) m

makeInterpreter :: [Int] -> Interpreter
makeInterpreter = Interpreter . M.fromList . zip [0..]

intcodeParser :: Parser Interpreter
intcodeParser = makeInterpreter <$> sepBy (signed hspace decimal) (char ',')
