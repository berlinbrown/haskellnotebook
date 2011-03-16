--
-- Author: Berlin Brown
-- File: DbReader.hs
--
-- Also see: 
-- (1) http://www.zvon.org/other/haskell/Outputio/index.html
-- (2) http://hackage.haskell.org/packages/archive/binary/0.4.1/doc/html/Data-Binary.html
--
-- Useful endian loading functions:
-- getWord8
-- getWord16be, getWord16le, getWord32be
-- getWord32le, getWord64be, getWord64le
--

module Main where

import Data.Word
import Data.Binary
import Data.Binary.Get as BinaryGet
import Data.Binary.Put as BinaryPut
import IO
import Text.Printf
import System.Environment

data SpiderDatabase =  SpiderDatabase { 
      magicNumberA :: Word16,
      magicNumberB :: Word16
    }

instance Show SpiderDatabase where
    show db = let (magica) = (magicNumberA db)
              in "magic number: [ 0x" ++ (show magica) ++ "]"

instance Binary SpiderDatabase where
    put _ = do BinaryPut.putWord16le 0
    get = do 
      magicnumbera <- BinaryGet.getWord16be
      magicnumberb <- BinaryGet.getWord16be
      return (SpiderDatabase {magicNumberA=magicnumbera,
                              magicNumberB=magicnumberb
                             })

main = do
  putStrLn "Running Spider Database Reader"
  args <- getArgs
  db :: SpiderDatabase <- decodeFile (args !! 0)
  putStrLn "<<<Database Content>>>"
  printf "%X %X\n" (magicNumberA db) (magicNumberB db)
  putStrLn "<<<End>>>"
  putStrLn "Done"
