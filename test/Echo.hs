{-# LANGUAGE OverloadedStrings
  #-}

import System.Process (runInteractiveProcess, waitForProcess, ProcessHandle)
import System.IO (Handle, stderr, stdout, stdin)
import Data.ByteString (ByteString, pack, hGetContents, hPutStrLn)
import Data.ByteString.Char8 (unpack)
import Data.Word
import Control.Monad

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Text.ShellEscape


prop_echoSh                 ::  Property
prop_echoSh                  =  monadicIO $ do
  b                         <-  pick arbitrary
  assert =<< run (echoTest (escape b :: Sh)) 


prop_echoBash               ::  Property
prop_echoBash                =  monadicIO $ do
  b                         <-  pick arbitrary
  assert =<< run (echoTest (escape b :: Bash)) 


echoTest escaped             =  do
  b                         <-  echo escaped
  when (b /= unescaped) $ do
    putStrLn "Echo result differs from unescaped result:"
    putStrLn "Escaped form:"
    putStrLn (show $ bytes escaped)
    putStrLn "Output of echo:"
    putStrLn (show b)
    putStrLn "Original:"
    putStrLn (show unescaped)
  return (b == unescaped)
 where
  unescaped                  =  unescape escaped

echo                        ::  (Shell t, Escape t) => t -> IO ByteString
echo escaped                 =  do
  (i, o, e, p)              <-  shell escaped cmd
  exit                      <-  waitForProcess p
  hGetContents o
 where
  cmd                        =  "echo -n " ++ unpack raw
  raw                        =  bytes escaped


class (Escape t) => Shell t where
  shell :: t -> String -> IO (Handle, Handle, Handle, ProcessHandle)
instance Shell Sh where
  shell _                    =  sh
instance Shell Bash where
  shell _                    =  bash


sh s = runInteractiveProcess "sh" ["-c", s] Nothing Nothing
bash s = runInteractiveProcess "bash" ["-c", s] Nothing Nothing

instance Arbitrary ByteString where
  arbitrary                  =  fmap (pack . filter (/= 0x00)) arbitrary

instance Arbitrary Word8 where
  arbitrary                  =  fmap fromIntegral (choose ( 0x00 :: Int
                                                          , 0xFF :: Int ))

main                         =  do
--hPutStrLn stderr "Testing Bourne Shell escaping."
--qc prop_echoSh
  hPutStrLn stderr "Testing Bash escaping."
  qc prop_echoBash
 where
  qcArgs                     =  Args Nothing 100000 100000 128
  qc                         =  quickCheckWith qcArgs

