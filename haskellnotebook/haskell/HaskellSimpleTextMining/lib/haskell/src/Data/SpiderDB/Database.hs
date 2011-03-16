-- ***********************************************
-- Author: Berlin Brown
-- File: Database.hs
--
-- Also see: 
-- (1) http://www.zvon.org/other/haskell/Outputio/index.html
-- (2) http://hackage.haskell.org/packages/archive/binary/0.4.1/doc/html/Data-Binary.html
-- (3) http://blog.kfish.org/2007/10/survey-haskell-unicode-support.html
-- (4) http://hackage.haskell.org/cgi-bin/hackage-scripts/package/utf8-string-0.2
-- (5) http://hackage.haskell.org/packages/archive/bytestring/0.9.0.1/doc/html/Data-ByteString.html
-- Useful endian loading functions:
-- getWord8, getWord16be, getWord32be
-- ***********************************************

module Data.SpiderDB.Database where

import Data.Word
import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as LazyC (unpack)
import Data.ByteString.Lazy as Lazy (ByteString, unpack)
import Data.ByteString (unpack)
--import Codec.Binary.UTF8.String as Unicode
import Data.Binary.Get as BinaryGet
import Data.Binary.Put as BinaryPut
import IO
import Text.Printf
import System.Environment
import Control.Monad (replicateM, liftM)

magicNUMBER_U2A = 0x05D4
magicNUMBER_U2B = 0xB0DB

majorNUMBER = 0x0001
minorNUMBER = 0x0000

headerDELIM = 0xFF07

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
      poolSize :: Word16,
      spiderPool :: [URLSet]
    }
data URLSet = URLSet {
      urlinfo :: URLInfo,
      titleinfo :: TitleInfo,
      descrinfo :: DescrInfo,
      keywordsinfo :: KeywordsInfo
}
data URLInfo = URLInfo {
      urltag :: Word8,
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
                  poolct = (poolSize db)
              in "<<<Database Content>>>\n" ++
                 (((printf "Magic: %X %X\n") (magicNumberA db)) (magicNumberB db)) ++
                 printf "URL Pool Count: %d\n" poolct ++
                 printf "Header Tag: %X\n" header ++
                 "<<<End>>>"
              where x = (spiderPool db)

instance Show URLSet where
    show urlset = printf "!URLSet@\n%s" (show (urlinfo urlset))

instance Show URLInfo where
    show urlinfo = printf "#URLInfo@%x, %d, %s"
                   (urltag urlinfo) (urlid urlinfo)
                   (LazyC.unpack (url urlinfo))
instance Show TitleInfo where
    show info = printf "#TitleInfo@%x, %s"
                (titletag info)
                (LazyC.unpack (title info))

instance Binary URLInfo where
    put ui = do      
      BinaryPut.putWord8 (urltag ui)
      BinaryPut.putWord16be (urlid ui)
      BinaryPut.putWord16be (urllen ui)
      BinaryPut.putLazyByteString (url ui)
    get = do
      utag <- getWord8
      idx <- getWord16be
      len <- getWord16be
      strdata <- BinaryGet.getLazyByteString (fromIntegral len)
      return (URLInfo {urltag=utag, urlid=idx, 
                       urllen=len, url=strdata})
instance Binary DescrInfo where
    put di = do
      BinaryPut.putWord8 (descrtag di)
      BinaryPut.putWord16be (descrlen di)      
      BinaryPut.putLazyByteString (descr di)
    get = do
      tag <- getWord8
      len <- getWord16be
      strdata <- BinaryGet.getLazyByteString (fromIntegral len)
      return (DescrInfo {descrtag=tag,
                         descrlen=len, 
                         descr=strdata})
instance Binary TitleInfo where
    put ti = do
      BinaryPut.putWord8 (titletag ti)
      BinaryPut.putWord16be (titlelen ti)      
      BinaryPut.putLazyByteString (title ti)
    get = do
      tag <- getWord8
      len <- getWord16be
      strdata <- BinaryGet.getLazyByteString (fromIntegral len)
      return (TitleInfo {titletag=tag,
                         titlelen=len, 
                         title=strdata})
instance Binary KeywordsInfo where
    put ki = do
      BinaryPut.putWord8 (keywordstag ki)
      BinaryPut.putWord16be (keywordslen ki)      
      BinaryPut.putLazyByteString (keywords ki)
    get = do
      tag <- getWord8
      len <- getWord16be
      strdata <- BinaryGet.getLazyByteString (fromIntegral len)
      return (KeywordsInfo {keywordstag=tag,
                            keywordslen=len, 
                            keywords=strdata})

instance Binary URLSet where
    put urlset = do
      put (urlinfo urlset)
      put (titleinfo urlset)
      put (descrinfo urlset)
      put (keywordsinfo urlset)
    get = do
      i <- (get :: Get URLInfo)
      j <- (get :: Get TitleInfo)
      k <- (get :: Get DescrInfo)
      x <- (get :: Get KeywordsInfo)
      return (URLSet {urlinfo=i, titleinfo=j, 
                      descrinfo=k, keywordsinfo=x})

instance Binary SpiderDatabase where
    put db = do
      BinaryPut.putWord16be (magicNumberA db)
      BinaryPut.putWord16be (magicNumberB db)
      BinaryPut.putWord16be (majorVers db)
      BinaryPut.putWord16be (minorVers db)
      BinaryPut.putWord16be (headerTag db)
      BinaryPut.putWord16be (poolSize db)
      -- @see mapM: Monad m => (a -> m b) -> [a] -> m [b]
      (mapM_ put (spiderPool db))
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
      pool1 <- replicateM (fromIntegral poolct) (get :: Get URLSet)
      return (SpiderDatabase {magicNumberA=magicnumbera,
                              magicNumberB=magicnumberb,
                              majorVers=major,
                              minorVers=minor,
                              headerTag=header,
                              poolSize=poolct,
                              spiderPool=pool1
                             })

-- *********************************************************
{-
  Utility Functions
 -}
-- *********************************************************

initSpiderDatabase :: [URLSet] -> SpiderDatabase
initSpiderDatabase urllist = SpiderDatabase {
                               magicNumberA=magicNUMBER_U2A,
                               magicNumberB=magicNUMBER_U2B,
                               majorVers=majorNUMBER,
                               minorVers=minorNUMBER,
                               headerTag=headerDELIM,
                               poolSize=(fromIntegral (length urllist)),
                               spiderPool=urllist
                             }

