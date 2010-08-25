{-# LANGUAGE OverloadedStrings
  #-}

import System.Environment
import System.Process (runInteractiveProcess, waitForProcess, ProcessHandle)
import System.IO (Handle, stderr, stdout, stdin)
import Data.ByteString (ByteString, pack, hGetContents, hPutStrLn)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Char8 as Char8 (pack)
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
    err "Echo result differs from unescaped result:"
    err "Escaped form:"
    err (Char8.pack . ("  "++) . show $ bytes escaped)
    err "Output of echo:"
    err (Char8.pack . ("  "++) $ show b)
    err "Original:"
    err (Char8.pack . ("  "++) $ show unescaped)
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
  arbitrary                  =  fmap pack arbitrary

instance Arbitrary Word8 where
  arbitrary                  =  fmap fromIntegral (choose ( 0x01 :: Int
                                                          , 0xFF :: Int ))

main                         =  do
  args                      <-  getArgs
  tests                     <-  case args of
                                  [arg]       ->  return $ read arg
                                  []          ->  return 100000
                                  _           ->  error "Invalid arguments."
  let msg                    =  "Performing " ++ show tests ++ " tests."
      qcArgs                 =  Args Nothing tests tests 128
      qc                     =  quickCheckWith qcArgs
  err "Tests are random ByteStrings, containing any byte but null."
--err "Testing Bourne Shell escaping."
--err (Char8.pack msg)
--qc prop_echoSh
  err "Testing Bash escaping."
  err (Char8.pack msg)
  qc prop_echoBash

err                          =  hPutStrLn stderr

