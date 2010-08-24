
module Text.ShellEscape.Bash where

import Data.Char
import Data.ByteString.Char8

import Text.ShellEscape.Escape
import qualified Text.ShellEscape.Put as Put


newtype Bash                 =  Bash (Vector (Char, EscapingMode))
 deriving (Eq, Ord, Show)

instance Escape Bash where
  escape b                   =  Bash . Vector.create $ do
    v                       <-  Vector.new (ByteString.length b)
    sequence_ . snd $ ByteString.foldl' (f v) (0, []) b
    return v
   where
    f v (i, ops) c           =  (i + 1, Vector.write v i (c, classify c) : ops)
  bytes (Bash b)             =  b


data EscapingMode            =  ANSIHex | ANSIBackslash | Literal
 deriving (Eq, Ord, Show)

renderANSI c =
  case classify c of
    Literal                 ->  Put.putChar     c
    ANSIHex                 ->  Put.putString $ hexify c
    ANSIBackslash           ->  Put.putString $ backslashify c

backslashify                ::  Char -> String
backslashify '\ESC'          =  "\\e"
backslashify c               =  (take 2 . drop 1 . show) c

hexify                      ::  Char -> String
hexify                       =  printf "\\x%02X" . ord

classify c | c <= '\ACK'     =  ANSIHex
           | c <= '\r'       =  ANSIBackslash
           | c <= '\SUB'     =  ANSIHex
           | c == '\ESC'     =  ANSIBackslash
           | c <= '\US'      =  ANSIHex
           | c <= '&'        =  Literal
           | c == '\''       =  ANSIBackslash
           | c <= '~'        =  Literal
           | c == '\DEL'     =  ANSIHex
           | otherwise       =  ANSIHex

