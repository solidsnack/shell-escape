
import Data.ByteString

import qualified Data.ByteString.ShellEscape as Esc


main                         =  do
  (options, args)           <-  getArgsAndOptions


data EscapingMode            =  Sh | Bash
 deriving (Eq, Ord, Show)
data Output                  =  Words | Lines
 deriving (Eq, Ord, Show)
data Input                   =  All | NullSeparated
 deriving (Eq, Ord, Show)


output Words                 =  out . intersperse 0x20
output Lines                 =  out . intersperse 0x0a

escape Sh args               =  mapM_ sh
escape Bash args             =  mapM_ bash

separate All                 =  (:[])
separate NullSeparated       =  split 0x00


out                          =  hPutStrLn stdout

err                          =  hPutStrLn stderr

