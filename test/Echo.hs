{-# LANGUAGE NoMonomorphismRestriction
  #-}

import System.Process
import Data.ByteString.Char8

import Test.QuickCheck

import Text.ShellEscape


echo                        ::  (Escape t) => t -> IO ByteString
echo escaped                 =  do
  (i, o, e, p)              <-  runInteractiveCommand cmd
  exit                      <-  waitForProcess p
  hGetContents o
 where
  cmd                        =  "echo -n " ++ (unpack . bytes) escaped

