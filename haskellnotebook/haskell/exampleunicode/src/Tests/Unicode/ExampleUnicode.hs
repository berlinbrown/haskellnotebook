-- ***********************************************
{-
  File: ExampleUnicode.hs
  Module: Test.Unicode.ExampleUnicode
  Author: Berlin Brown
  Date: 1/10/2008
  Compiler: GHC

  References:
  (1) http://haskell.org/ghc/docs/latest/html/libraries/
  (2) http://haskell.org/ghc/docs/latest/html/libraries/base/System-IO.html
  (3) http://hackage.haskell.org/packages/archive/utf8-string/0.2/doc/html/Codec-Binary-UTF8-String.html
 -}
-- ***********************************************

module Main where

import System.IO
import System ( getArgs )
import Data.ByteString as BS (hGetContents, unpack)
import Codec.Binary.UTF8.String as UTF (decode)

main = do
  putStrLn "Running Example Unicode Test"
  args <- getArgs
  h <- openBinaryFile (args !! 0) ReadMode
  -- Contents returned as bytestring
  contents <- BS.hGetContents h
  -- Convert bytestring data to Word8(byte) array
  let data_word8 = BS.unpack(contents)
      unicode = UTF.decode(data_word8)
  putStrLn $ "UTF8 String:" ++ unicode
  hClose h
  putStrLn "Done"