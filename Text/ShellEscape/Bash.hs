
module Text.ShellEscape.Bash where

import Data.Maybe
import Data.Char
import Text.Printf
import Data.ByteString (ByteString)

import qualified Data.Vector as Vector

import Text.ShellEscape.Escape
import qualified Text.ShellEscape.Put as Put
import Text.ShellEscape.EscapeVector


newtype Bash                 =  Bash (EscapeVector EscapingMode)
 deriving (Eq, Ord, Show)


instance Escape Bash where
  escape                     =  Bash . escWith classify
  unescape (Bash v)          =  stripEsc v
  bytes (Bash v) | literal v =  stripEsc v
                 | otherwise =  interpretEsc v renderANSI' end (begin, Literal)
   where
    literal                  =  isNothing . Vector.find ((/= Literal) . snd)
    begin                    =  [      Put.putString "$'"]
    end                      =  const (Put.putChar '\'')
    renderANSI' _ (c, e)     =  (renderANSI c, e)


data EscapingMode            =  ANSIHex | ANSIBackslash | Literal | Quoted
 deriving (Eq, Ord, Show)

renderANSI c =
  case classify c of
    Literal                 ->  Put.putChar     c
    Quoted                  ->  Put.putChar     c
    ANSIHex                 ->  Put.putString $ hexify c
    ANSIBackslash           ->  Put.putString $ backslashify c

backslashify                ::  Char -> String
backslashify '\ESC'          =  "\\e"
backslashify c               =  (take 2 . drop 1 . show) c

hexify                      ::  Char -> String
hexify                       =  printf "\\x%02X" . ord

classify                    ::  Char -> EscapingMode
classify c | c <= '\ACK'     =  ANSIHex
           | c <= '\r'       =  ANSIBackslash
           | c <= '\SUB'     =  ANSIHex
           | c == '\ESC'     =  ANSIBackslash
           | c <= '\US'      =  ANSIHex
           | c <= '&'        =  Quoted
           | c == '\''       =  ANSIBackslash
           | c <= '+'        =  Quoted
           | c <= '9'        =  Literal
           | c <= '?'        =  Quoted
           | c <= 'Z'        =  Literal
           | c == '['        =  Quoted
           | c == '\\'       =  ANSIBackslash
           | c <= '`'        =  Quoted
           | c <= 'z'        =  Literal
           | c <= '~'        =  Quoted
           | c == '\DEL'     =  ANSIHex
           | otherwise       =  ANSIHex

