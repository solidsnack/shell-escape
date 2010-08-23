
module Text.ShellEscape.Bash where


import Data.ByteString.Char8

import Text.ShellEscape.Escape


newtype Bash                 =  Bash ByteString
 deriving (Eq, Ord, Show)

instance Escape Bash where
  escape b                   =  Bash b
  bytes (Bash b)             =  b

