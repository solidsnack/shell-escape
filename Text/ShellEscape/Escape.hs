
module Text.ShellEscape.Escape where


import Data.ByteString


class Escape t where
  escape                    ::  ByteString -> t
  unescape                  ::  t -> ByteString
  bytes                     ::  t -> ByteString

instance Escape ByteString where
  escape                     =  id
  unescape                   =  id
  bytes                      =  id

