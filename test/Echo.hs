{-# LANGUAGE OverloadedStrings
  #-}

import System.Environment
import System.Process (runInteractiveProcess, waitForProcess, ProcessHandle)
import System.IO (Handle, stderr, stdout, stdin)
import Data.ByteString (ByteString, pack, hGetContents, hPutStrLn)
import qualified Data.ByteString
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Char8 as Char8
import Data.Word
import Control.Monad
import Text.Printf

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Text.ShellEscape


prop_echoBash               ::  ByteString -> Property
prop_echoBash                =  something_prop escapeBash

prop_echoSh                 ::  ByteString -> Property
prop_echoSh                  =  something_prop escapeSh

something_prop esc b         =  monadicIO (assert =<< run (echoTest (esc b)))

escapeSh                    ::  ByteString -> Sh
escapeSh b                   =  escape b

escapeBash                  ::  ByteString -> Bash
escapeBash b                 =  escape b

echoTest escaped             =  do
  b                         <-  echo escaped
  Data.ByteString.appendFile "./lengths" (displayLength unescaped)
  Data.ByteString.appendFile "./chars" (displayBytes unescaped)
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


displayBytes                 =  Char8.pack . unlines
                             .  fmap (\w ->  printf "0x%02x" w)
                             .  Data.ByteString.unpack

displayLength                =  Char8.pack
                             .  (\w ->  printf "%4d\n" w)
                             .  Data.ByteString.length

class (Escape t) => Shell t where
  shell :: t -> String -> IO (Handle, Handle, Handle, ProcessHandle)
instance Shell Sh where
  shell _                    =  sh
instance Shell Bash where
  shell _                    =  bash


sh s = runInteractiveProcess "sh" ["-c", s] Nothing Nothing
bash s = runInteractiveProcess "bash" ["-c", s] Nothing Nothing

instance Arbitrary ByteString where
  arbitrary                  =  do
    bytes                   <-  arbitrary :: Gen [Word8]
    NonEmpty bytes'         <-  arbitrary :: Gen (NonEmptyList Word8)
    pack `fmap` elements [bytes', bytes', bytes, bytes', bytes']

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
      qcArgs                 =  Args Nothing tests tests 32
      qc                     =  quickCheckWith qcArgs
  err "Tests are random ByteStrings, containing any byte but null."
--err "Testing Bourne Shell escaping."
--err (Char8.pack msg)
--qc prop_echoSh
  err "Testing Bash escaping."
  err (Char8.pack msg)
  qc prop_echoBash

err                          =  hPutStrLn stderr

