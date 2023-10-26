module Solutions.Year2015.Day7 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Shared

newtype Wire = Wire String deriving Show
data Gate =
    And Wire Wire |
    Or Wire Wire |
    Not Wire |
    Lshift Wire Int |
    Rshift Wire Int |
    Const Int
     deriving Show
data Connection = Connection Gate Wire deriving Show

parser :: Parser [Connection]
parser = sepBy connection newline
    where connection = Connection <$> gate <* string " -> " <*> wire
          wire = Wire <$> many lowerChar
          gate = try (And <$> wire <* string " AND " <*> wire) <|>
                 try (Or <$> wire <* string " OR " <*> wire) <|>
                 try (Not <$> (string "NOT " *> wire)) <|>
                 try (Lshift <$> wire <* string " LSHIFT " <*> decimal) <|>
                 (Rshift <$> wire <* string " RSHIFT " <*> decimal) <|>
                 (Const <$> decimal)


solution = runSolution parser (const 42) id
