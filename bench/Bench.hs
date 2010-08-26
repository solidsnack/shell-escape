
{-# LANGUAGE OverloadedStrings
  #-}

import Data.ByteString.Char8

import Criterion.Main

import Text.ShellEscape


strings                     ::  [ByteString]
strings =
  [ "echo * $PWD"
  , ""
  , "~/Music/M.I.A. & Diplo - Piracy Funds Terrorism Vol. 1 (2004)"
  , "abcds"
  , "\x00\n\204\DEL"
  , "\x00\n\204\DELecho * $PWD" ]

main                         =  defaultMain $ fmap bench' strings
 where
  bench' s                   =  bench (show s) (whnf escape' s)
  escape'                   ::  ByteString -> Bash
  escape'                    =  escape

