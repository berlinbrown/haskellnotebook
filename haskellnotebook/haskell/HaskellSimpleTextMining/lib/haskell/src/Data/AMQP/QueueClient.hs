{- ***********************************************
   File:    QueueClient.hs
   Module:  Data.AMQP.QueueClient
   Date:    1/10/2008
   Author:  Berlin Brown

   Description: 
   Simple connection to AMQP Server (like RabbitMQ).

   See the following reference links:
   (1) http://www.haskell.org/ghc/docs/6.8.2/html/libraries/network/Network.html
   (2) http://hackage.haskell.org/packages/archive/binary/0.4.1/doc/html/Data-Binary.html
   (3) http://hackage.haskell.org/packages/archive/bytestring/0.9.0.1/doc/html/Data-ByteString.html
 
   Based on python AMQP Client Library
   from Barry Pederson.
   (4) http://barryp.org/software/py-amqplib/   
   *********************************************** -}
module Data.AMQP.QueueClient where

import Network
import System.IO

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
import qualified Codec.Binary.UTF8.String as UTF (encode, decode)

import Data.AMQP.AMQPOperations (methodNameMap)

ampqClientVers = "0.0.1"

server = "127.0.0.1"
port = 5672

amqpHeadA = "AMQP"
amqpHeadB = 0x01010901

amqPlainMethod = "AMQPlain"
localeEnUs = "en_US"


-- *********************************************************
{-
  AMQP Utility Functions
-}
-- *********************************************************

--
-- | Convert Lazy(file loaded) ByteString data to 
-- regular UTF-8 string and then to Eager ByteString.  We do this
-- so that we can easily read or write data to the network.
{-
  Eager.pack :: [Word8] -> ByteString
  CharBS.pack :: String -> ByteString
  Lazy.unpack :: ByteString -> [Word8]
 -}
convertToByteString :: Lazy.ByteString -> Eager.ByteString
convertToByteString bs = CharBS.pack . convertToString $ bs

convertToString :: Lazy.ByteString -> String
convertToString bs = UTF.decode . Lazy.unpack $ bs

-- | Convert a normal string to UTF8 (encode) and then to an Eager ByteString
stringToByteString :: String -> Eager.ByteString
stringToByteString str = Eager.pack . UTF.encode $ str

-- | Convert a ByteString to AMQP String
stringToAMQPls :: String -> AMQPLongStr
stringToAMQPls str = (lenbs, bsdata)
    where bsdata = (stringToByteString str) 
          lenbs  = (fromIntegral (length (Eager.unpack bsdata)))
stringToAMQPss :: String -> AMQPShortStr
stringToAMQPss str = (lenbs, bsdata)
    where bsdata = (stringToByteString str)
          lenbs  = (fromIntegral (length (Eager.unpack bsdata)))

--
-- Take and Drop for ByteStrings; take N Word8 elements and then rebuild
-- the bytestring.
-- This will be used to build data structures from parts of bytestring
-- data.
takeByteString :: Int -> Lazy.ByteString -> Lazy.ByteString
takeByteString n lazybs = (Lazy.pack (take n (Lazy.unpack lazybs)))

-- *********************************************************
{-
  AMQP Data Types.
-}
-- *********************************************************
type Octet = Word8

-- | Note, a AMQP String type consists of a length value (after encoding)
-- and the bytestring data tuple.
type AMQPLongStr = (Word32, Eager.ByteString)
type AMQPShortStr = (Word16, Eager.ByteString)

data AMQPData = AMQPData {
      amqpHeaderA :: [Octet],
      amqpHeaderB :: Word32
}

data AMQPStartOk = AMQPStartOk {
      -- *****************************************
      {- 
         shortstr = Up to 255 bytes, after encoding (len:byte)
         longstr = Write a string up to 2 ^ 32 bytes after encoding (len:long)
       -}
      -- *****************************************
      mechanism :: AMQPShortStr,
      response :: AMQPLongStr,
      locale :: AMQPShortStr
}

initStartOk :: AMQPStartOk
initStartOk = AMQPStartOk {
                mechanism = (stringToAMQPss amqPlainMethod),
                response = (stringToAMQPls ""),
                locale = (stringToAMQPss localeEnUs)
              }

-- *********************************************************
{-
   Wait for a frame:
   1. Frame Type (Octet)
   2. Channel (Short)
   3. Size (Long)
   4. Payload (variable length, set to size)
   5. ch
-}
-- *********************************************************
data AMQPFrame = AMQPFrame {
      frameType :: Octet,
      channel :: Word16,
      size :: Word32,
      payload :: Lazy.ByteString,
      ch :: Octet
}

data AMQPClassMethod = AMQPClassMethod {
      classId :: Word16,
      methodId :: Word16
}

{- *********************************************************
     Class instances
   ********************************************************* -}

--
-- | Write the AMQP String to the network
-- The AMQP String tuple contains the following (length, data).
-- First, output the length value and then the bytestring content.
-- @see type Put = PutM ()
putAMQPStringLs :: (Word32, Eager.ByteString) -> Put
putAMQPStringLs amqstr =
    writeStrLen >> BinaryPut.putByteString (snd amqstr) 
    where writeStrLen = BinaryPut.putWord32be (fromIntegral (fst amqstr))
putAMQPStringSs :: (Word16, Eager.ByteString) -> Put
putAMQPStringSs amqstr =
    writeStrLen >> BinaryPut.putByteString (snd amqstr) 
    where writeStrLen = BinaryPut.putWord16be (fromIntegral (fst amqstr))

instance Show AMQPFrame where
    show amq = "<<<AMQP Reader>>>\n" ++
               printf "FrameType: %X\n" (frameType amq) ++
               printf "Channel: %X\n" (channel amq) ++
               printf "Size: %d\n" (size amq) ++
               printf "Ch: %X\n" (ch amq)

instance Show AMQPClassMethod where
    show amqclass = printf "operation={ Class Id:%d, Method Id:%d }" 
                    (classId amqclass) (methodId amqclass)

instance Binary AMQPStartOk where
    put amqStartOk = do
      putAMQPStringSs (mechanism amqStartOk)
      putAMQPStringLs (response amqStartOk)
      putAMQPStringSs (locale amqStartOk)
               
instance Binary AMQPFrame where
    get = do
      frameType <- getWord8
      chan <- getWord16be
      sz <- getWord32be
      bytes <- BinaryGet.getLazyByteString (fromIntegral sz)
      chw <- getWord8
      return (AMQPFrame { 
                frameType=frameType,
                channel=chan,
                size=sz,
                payload=bytes,
                ch=chw
              })

instance Binary AMQPClassMethod where
    get = do
      sigclass <- getWord16be
      sigmethod <- getWord16be
      return (AMQPClassMethod {
                classId = sigclass,
                methodId = sigmethod
              })

instance Binary AMQPData where

    -- *****************************************************
    {-
    The connection class provides methods for a client to establish a
    network connection to a server, and for both peers to operate the
    connection thereafter.

    GRAMMAR:

        connection          = open-connection *use-connection close-connection
        open-connection     = C:protocol-header
                              S:START C:START-OK
                              *challenge
                              S:TUNE C:TUNE-OK
                              C:OPEN S:OPEN-OK | S:REDIRECT
        challenge           = S:SECURE C:SECURE-OK
        use-connection      = *channel
        close-connection    = C:CLOSE S:CLOSE-OK
                            / S:CLOSE C:CLOSE-OK

     Additional Notes:
     BinaryPut.putByteString :: Eager.ByteString -> Put
     Usage: (convert regular string to BS): BinaryPut.putByteString (Eager.pack theStr)
     -}
     -- *******************************************************
    put amq = do
      BinaryPut.putByteString (Eager.pack (amqpHeaderA amq))
      BinaryPut.putWord32be (amqpHeaderB amq)

amqInstance :: IO AMQPData
amqInstance = return (AMQPData { 
                        amqpHeaderA = (Eager.unpack (CharBS.pack amqpHeadA)),
                        amqpHeaderB = amqpHeadB
                      })

connectSimpleServer = do
  -- Create an instance of the AMQ data to send
  -- across the network.
  amq <- amqInstance    

  -- Connect to the given server through hostname and port number
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  
  -- Convert the instance of the data into a lazy bytestring type
  let bs_amq = encode amq
  -- Through the use of lazy hPut, write out the data to the socket handle
  Lazy.hPut h bs_amq
  hFlush h

  -- Wait for frame
  bs_reader <- Lazy.hGetContents h
  let amqFrame = decode bs_reader :: AMQPFrame
  putStrLn $ show(amqFrame)
  
  -- Extract the payload, checking the method signature
  let framePayload = (payload amqFrame)
      operationSig = decode (takeByteString 4 framePayload) :: AMQPClassMethod

  -- Get the method signature values, we expect 10,10
  putStrLn $ show(operationSig)
  putStrLn $ methodNameMap ((fromIntegral (classId operationSig)), 
                            (fromIntegral (methodId operationSig)))

  -- Write the startok binary string data
  let bs_startok = encode initStartOk
  
  t <- hGetContents h
  print t
