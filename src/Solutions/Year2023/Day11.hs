module Solutions.Year2023.Day11 where

import Control.Monad (void)

import Shared
import Text.Megaparsec (MonadParsec(eof))

solution = runSolution eof (const ()) (const ())