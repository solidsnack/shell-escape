
module Text.ShellEscape.Sh where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as Vector

import Text.ShellEscape.Escape
import qualified Text.ShellEscape.Put as Put


newtype Sh                   =  Sh (Vector (Char, EscapingMode))
 deriving (Eq, Ord, Show)

instance Escape Sh where
  escape b                   =  Sh . Vector.create $ do
    v                       <-  Vector.new (ByteString.length b)
    sequence_ . snd $ ByteString.foldl' (f v) (0, []) b
    return v
   where
    f v (i, ops) c           =  (i + 1, Vector.write v i (c, classify c) : ops)
  unescape (Sh v)            =  ByteString.unfoldr f . fst $ Vector.unzip v
   where
    f v | Vector.null v      =  Nothing
        | otherwise          =  Just (Vector.unsafeHead v, Vector.unsafeTail v)
  bytes (Sh v)               =  eval instructions
   where
    eval                     =  fin . Put.runPut' . sequence_ . reverse
    (instructions, final)    =  Vector.foldl' f ([], Literal) v
     where
      f (list, mode) e       =  (put:list, mode')
       where
        (put, mode')         =  act mode e
    fin | final == Quote     =  (`ByteString.snoc` '\'')
        | final == Backslash =  (`ByteString.snoc` '\\')
        | otherwise          =  id


--  Accept the present escaping mode and desired escaping mode and yield an
--  action and the resulting mode.
act :: EscapingMode -> (Char, EscapingMode) -> (Put.Put, EscapingMode)
act Quote (c, Quote)         =  (Put.putChar c                 , Quote)
act Quote (c, Literal)       =  (Put.putString ['\'', c]       , Literal)
act Quote (c, Backslash)     =  (Put.putString ['\'', '\\', c] , Literal)
act Backslash (c, Backslash) =  (Put.putChar c                 , Literal)
act Backslash (c, Quote)     =  (Put.putString ['\\', '\'', c] , Quote)
act Backslash (c, Literal)   =  (Put.putString ['\\', c]       , Literal)
act Literal (c, Literal)     =  (Put.putChar c                 , Literal)
act Literal (c, Backslash)   =  (Put.putString ['\\', c]       , Literal)
act Literal (c, Quote)       =  (Put.putString ['\'', c]       , Quote)

classify                    ::  Char -> EscapingMode
classify c | c <= '&'        =  Quote           --  0x00..0x26
           | c == '\''       =  Backslash       --  0x27
           | c <= ','        =  Quote           --  0x28..0x2c
           | c <= '9'        =  Literal         --  0x2d..0x39
           | c <= '?'        =  Quote           --  0x3a..0x3f
           | c <= 'Z'        =  Literal         --  0x40..0x5a
           | c <= '^'        =  Quote           --  0x5b..0x5e
           | c == '_'        =  Literal         --  0x5f
           | c == '`'        =  Quote           --  0x60
           | c <= 'z'        =  Literal         --  0x61..0x7a
           | c <= '\DEL'     =  Quote           --  0x7b..0x7f
           | otherwise       =  Quote           --  0x80..0xff

data EscapingMode            =  Backslash | Literal | Quote
 deriving (Eq, Ord, Show)


{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  \NUL \SOH \STX \ETX \EOT \ENQ \ACK \a \b \t \n \v \f \r \SO \SI \DLE
  \DC1 \DC2 \DC3 \DC4 \NAK \SYN \ETB \CAN \EM \SUB \ESC \FS \GS \RS \US ' '
  ! " # $ % & ' ( ) * + , - .  / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?  @
  A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \\ ] ^ _ ` a b c
  d e f g h i j k l m n o p q r s t u v w x y z { | } ~ \DEL

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}



