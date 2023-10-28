module Solutions.Year2015.Day7 where

import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import qualified Data.Map as Map
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Shared

type Wire = String

data Value = Wire Wire | Const Word16

data Gate =
    And Value Value |
    Or Value Value |
    Not Value |
    Lshift Value Int |
    Rshift Value Int |
    Value Value

type Circuit = Map.Map Wire Gate

-- helper functions
dependencies :: Gate -> [Wire]
dependencies gate = case gate of
    And v1 v2 -> wires [v1, v2]
    Or v1 v2 -> wires [v1, v2]
    Not v1 -> wires [v1]
    Lshift v1 n -> wires [v1]
    Rshift v1 b -> wires [v1]
    Value v1 -> wires [v1]

wires :: [Value] -> [Wire]
wires = map getWire . filter isWire

getWire :: Value -> Wire
getWire (Wire w) = w

isWire :: Value -> Bool
isWire (Wire _) = True
isWire (Const _) = False

-- parser stuff
buildCircuit :: [(Gate, Wire)] -> Circuit
buildCircuit = foldr (uncurry . flip $ Map.insert) Map.empty

parser :: Parser Circuit
parser = buildCircuit <$> sepBy connection newline
    where connection = (,) <$> gate <* string " -> " <*> wire
          wire = many lowerChar
          value = (Const <$> decimal) <|> (Wire <$> wire)
          gate = try (And <$> value <* string " AND " <*> value) <|>
                 try (Or <$> value <* string " OR " <*> value) <|>
                 try (Not <$> (string "NOT " *> value)) <|>
                 try (Lshift <$> value <* string " LSHIFT " <*> decimal) <|>
                 try (Rshift <$> value <* string " RSHIFT " <*> decimal) <|>
                 (Value <$> value)

-- actual solution
type CircuitState = Map.Map Wire Word16

buildState :: Circuit -> CircuitState
buildState circuit = calc Map.empty . Map.keys $ circuit
    where calc state [] = state
          calc state (wire:xs) = if any (`Map.notMember` state) . dependencies $ (Map.!) circuit wire
                                 then calc state (xs ++ [wire])
                                 else calc (Map.insert wire (calcSingleWire state wire) state) xs
          calcSingleWire state wire = case (Map.!) circuit wire of
                                          And w1 w2 -> apply2 state (.&.) w1 w2
                                          Or w1 w2 -> apply2 state (.|.) w1 w2
                                          Not w -> apply1 state complement w
                                          Lshift w n -> apply1 state (`shiftL` n) w
                                          Rshift w n -> apply1 state (`shiftR` n) w
                                          Value n -> getValue state n
          getValue state (Const n) = n
          getValue state (Wire w) = state Map.! w
          apply1 state f v = f (getValue state v)
          apply2 state f v1 v2 = f (getValue state v1) (getValue state v2)


solve1 :: Circuit -> Word16
solve1 = (Map.! "a") . buildState

solve2 :: Circuit -> Word16
solve2 circuit = (Map.! "a") . buildState . Map.insert "b" (Value (Const (solve1 circuit))) $ circuit

solution = runSolution parser solve1 solve2
