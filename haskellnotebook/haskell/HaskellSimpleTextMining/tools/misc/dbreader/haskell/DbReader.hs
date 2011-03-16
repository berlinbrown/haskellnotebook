--
-- Author: Berlin Brown
-- File: DbReader.hs
--
-- Also see: 
-- (1) http://www.zvon.org/other/haskell/Outputio/index.html
-- (2) http://hackage.haskell.org/packages/archive/binary/0.4.1/doc/html/Data-Binary.html
-- (3) http://blog.kfish.org/2007/10/survey-haskell-unicode-support.html
-- (4) http://hackage.haskell.org/cgi-bin/hackage-scripts/package/utf8-string-0.2
-- (5) http://hackage.haskell.org/packages/archive/bytestring/0.9.0.1/doc/html/Data-ByteString.html
-- Useful endian loading functions:
-- getWord8, getWord16be, getWord32be
--

module Main where

import Data.Word
import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as LazyChar8 (unpack)
import Data.ByteString.Lazy as Lazy (ByteString, unpack)
import Data.ByteString (unpack)
--import Codec.Binary.UTF8.String as Unicode
import Data.Binary.Get as BinaryGet
import Data.Binary.Put as BinaryPut
import IO
import Text.Printf
import System.Environment
import Control.Monad (replicateM, liftM)

{- *********************************************************
     Define the Database Data Types
     SpiderDatabase represents a singleton wrapper for an
     entire database.
   ********************************************************* -}
data SpiderDatabase =  SpiderDatabase { 
      magicNumberA :: Word16,
      magicNumberB :: Word16,
      majorVers :: Word16,
      minorVers :: Word16,
      headerTag :: Word16,
      poolLen :: Word16,
      spiderpool :: [URLSet]
    }
data URLSet = URLSet {
      urlinfo :: URLInfo,
      titleinfo :: TitleInfo,
      descrinfo :: DescrInfo,
      keywordsinfo :: KeywordsInfo
}
data URLInfo = URLInfo {
      tag :: Word8,
      urlid :: Word16,
      urllen :: Word16,
      url :: ByteString
}
data TitleInfo = TitleInfo {
      titletag :: Word8,      
      titlelen :: Word16,
      title :: ByteString
}
data DescrInfo = DescrInfo {
      descrtag :: Word8,      
      descrlen :: Word16,
      descr :: ByteString
}
data KeywordsInfo = KeywordsInfo {
      keywordstag :: Word8,      
      keywordslen :: Word16,
      keywords :: ByteString
}
{- *********************************************************
     Class instances
   ********************************************************* -}
instance Show SpiderDatabase where
    show db = let magicb = (magicNumberB db)
                  header = (headerTag db)
                  poolct = (poolLen db)
              in "<<<Database Content>>>\n" ++
                 (((printf "Magic: %X %X\n") (magicNumberA db)) (magicNumberB db)) ++
                 printf "URL Pool Count: %d\n" poolct ++
                 printf "Header Tag: %X\n" header ++
                 printf "URL Tag %X\n" a ++
                 printf "URL Idx %X\n" b ++
                 printf "URL: %s\n" c ++
                 printf "Title: %s\n" e ++
                 "<<<End>>>"
              where x = (spiderpool db)
                    y = (x !! 0)
                    z = (urlinfo y)
                    a = (tag z)
                    b = (urlid z)
                    c = (LazyChar8.unpack (url z))
                    d = (titleinfo y)
                    e = (LazyChar8.unpack (title d))

instance Binary URLInfo where
    put _ = do BinaryPut.putWord8 0
    get = do
      urltag <- getWord8
      idx <- getWord16be
      len <- getWord16be
      strdata <- BinaryGet.getLazyByteString (fromIntegral len)
      return (URLInfo {tag=urltag, urlid=idx, 
                       urllen=len, url=strdata})
instance Binary DescrInfo where
    put _ = do BinaryPut.putWord8 0
    get = do
      tag <- getWord8
      len <- getWord16be
      strdata <- BinaryGet.getLazyByteString (fromIntegral len)
      return (DescrInfo {descrtag=tag,
                         descrlen=len, 
                         descr=strdata})
instance Binary TitleInfo where
    put _ = do BinaryPut.putWord8 0
    get = do
      tag <- getWord8
      len <- getWord16be
      strdata <- BinaryGet.getLazyByteString (fromIntegral len)
      return (TitleInfo {titletag=tag,
                         titlelen=len, 
                         title=strdata})
instance Binary KeywordsInfo where
    put _ = do BinaryPut.putWord8 0
    get = do
      tag <- getWord8
      len <- getWord16be
      strdata <- BinaryGet.getLazyByteString (fromIntegral len)
      return (KeywordsInfo {keywordstag=tag,
                            keywordslen=len, 
                            keywords=strdata})

instance Binary URLSet where
    put _ = do BinaryPut.putWord8 0
    get = do
      i :: URLInfo <- get :: Get URLInfo
      j :: TitleInfo <- get :: Get TitleInfo
      k :: DescrInfo <- get :: Get DescrInfo
      x :: KeywordsInfo <- get :: Get KeywordsInfo
      return (URLSet {urlinfo=i, titleinfo=j, 
                      descrinfo=k, keywordsinfo=x})

getURLSet :: Get URLSet
getURLSet = get :: Get URLSet

instance Binary SpiderDatabase where
    put _ = do BinaryPut.putWord8 0
    get = do 
      magicnumbera <- BinaryGet.getWord16be
      magicnumberb <- BinaryGet.getWord16be
      major <- BinaryGet.getWord16be
      minor <- BinaryGet.getWord16be
      header <- BinaryGet.getWord16be
      poolct <- BinaryGet.getWord16be
      -- *******************************
      -- Get the remaining byte string data,
      -- So that we can use lazy bytestring to load to load the
      -- the data types.
      -- *******************************
      pool1 <- getURLSet
      return (SpiderDatabase {magicNumberA=magicnumbera,
                              magicNumberB=magicnumberb,
                              majorVers=major,
                              minorVers=minor,
                              headerTag=header,
                              poolLen=poolct,
                              spiderpool=(pool1 : [])
                             })
main = do
  putStrLn "Running Spider Database Reader"
  args <- getArgs
  db :: SpiderDatabase  <- decodeFile (args !! 0)  
  putStrLn $ show db
  putStrLn "Done"
