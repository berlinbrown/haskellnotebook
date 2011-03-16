-- ***********************************************
-- Author: Berlin Brown
-- File: Queue.hs
-- Date: 1/10/2008
--
-- Description:
-- Simple Queue Binary File Format Database.
--
-- References:
-- (1) http://hackage.haskell.org/packages/archive/binary/0.4.1/doc/html/Data-Binary.html
-- (2) http://hackage.haskell.org/cgi-bin/hackage-scripts/package/utf8-string-0.2
-- (3) http://hackage.haskell.org/packages/archive/bytestring/0.9.0.1/doc/html/Data-ByteString.html
-- (4) http://www.haskell.org/ghc/docs/latest/html/libraries/haskell98/Time.html
-- ***********************************************

module Data.SpiderQueue.Queue where

import System.IO
import Control.Monad (replicateM, forM, liftM)
import Data.Word
import Data.Binary
import Data.Binary.Get as BinaryGet
import Data.Binary.Put as BinaryPut
import Text.Printf

--
-- Used qualified names for the different bytestring manipulation
-- modules; using 'Import Qualified' to ensure we are using the correct function.
import qualified Data.ByteString as Eager (ByteString, unpack, pack)
import qualified Data.ByteString.Char8 as CharBS (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as LazyC (unpack, pack)
import qualified Data.ByteString.Lazy as Lazy (ByteString, unpack, pack, hPut, hGetContents)

magicNUMBER_U2A = 0x25D4
magicNUMBER_U2B = 0xB0DB

majorNUMBER = 0x0001
minorNUMBER = 0x0000

queueTAG = 0x79

--
-- | Simple First in, First Out binary file format
data SpiderQueue = SpiderQueue { 
      magicNumberA :: Word16,
      magicNumberB :: Word16,
      majorVers :: Word16,
      minorVers :: Word16,
      queueSize :: Word32,
      queue :: [QueueObject]
}
data QueueObject = QueueObject {
      segtag :: Word8,
      dbseglen :: Word32,                  
      dbsegment :: Lazy.ByteString,
      -- Posix time
      ptime :: Word32
}

instance Show SpiderQueue where
    show db =  "<<<Database Content>>>\n" ++
               printf " Magic: %X %X\n" (magicNumberA db)
                          (magicNumberB db)

instance Show QueueObject where
    show obj =  "<<<Queue Content>>>\n" ++
               printf " Tag: %X Len: %d\n" (segtag obj)
                          (dbseglen obj)

instance Binary QueueObject where
    put dbq = do
      BinaryPut.putWord8 (segtag dbq)
      BinaryPut.putWord32be (dbseglen dbq)
      BinaryPut.putLazyByteString (dbsegment dbq)
      BinaryPut.putWord32be (ptime dbq)
    get = do
      tag <- getWord8
      len <- getWord32be
      segdata <- BinaryGet.getLazyByteString (fromIntegral len)
      t <- getWord32be
      return (QueueObject {
                segtag=tag, dbseglen=len, 
                dbsegment=segdata, ptime=t
              })

instance Binary SpiderQueue where
    put dbq = do 
      BinaryPut.putWord16be (magicNumberA dbq)
      BinaryPut.putWord16be (magicNumberB dbq)
      BinaryPut.putWord16be (majorVers dbq)
      BinaryPut.putWord16be (minorVers dbq)
      BinaryPut.putWord32be (queueSize dbq)
      -- @see mapM: Monad m => (a -> m b) -> [a] -> m [b]
      (mapM_ put (queue dbq))
    get = do 
      magicnumbera <- BinaryGet.getWord16be
      magicnumberb <- BinaryGet.getWord16be
      major <- BinaryGet.getWord16be
      minor <- BinaryGet.getWord16be
      len <- BinaryGet.getWord32be
      -- *******************************
      -- Get the remaining byte string data,
      -- So that we can use lazy bytestring to load to load the
      -- the data types.
      -- Also: queueData <- forM [1..len] (const (get :: Get QueueObject))
      -- *******************************
      queueData <- replicateM (fromIntegral len) (get :: Get QueueObject)
      return (SpiderQueue {magicNumberA=magicnumbera,
                           magicNumberB=magicnumberb,
                           majorVers=major,
                           minorVers=minor,
                           queueSize=len,
                           queue=queueData
                          })

-- *********************************************************
{-
  Utility Functions
 -}
-- *********************************************************
initSpiderQueue :: [QueueObject] -> SpiderQueue
initSpiderQueue objlist = SpiderQueue {
                            magicNumberA=magicNUMBER_U2A,
                            magicNumberB=magicNUMBER_U2B,
                            majorVers=majorNUMBER,
                            minorVers=minorNUMBER,
                            queueSize=(fromIntegral (length objlist)),
                            queue=objlist
                          }

initQueueObject :: String -> Integer -> QueueObject
initQueueObject obj t = QueueObject {
                        segtag=(fromIntegral queueTAG),
                        dbseglen=(fromIntegral lenbs),
                        dbsegment=bs,
                        ptime=pt }
                  where
                    bs = LazyC.pack obj
                    lenbs = length . LazyC.unpack $ bs
                    pt = (fromIntegral t)
