
import Data.ByteString

import qualified Text.ShellEscape as Esc


main                         =  do
  args                      <-  getArgs


data EscapingMode            =  Sh | Bash
 deriving (Eq, Ord, Show)
data Output                  =  Words | Lines
 deriving (Eq, Ord, Show)
data Input                   =  All | NullSeparated
 deriving (Eq, Ord, Show)

