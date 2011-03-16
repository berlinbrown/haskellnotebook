{- ***********************************************
   File: SimpleClient.hs
   See the following Reference links:
   (1) http://www.haskell.org/ghc/docs/6.8.2/html/libraries/network/Network.html
   (2) http://hackage.haskell.org/packages/archive/binary/0.4.1/doc/html/Data-Binary.html
   (3) http://hackage.haskell.org/packages/archive/bytestring/0.9.0.1/doc/html/Data-ByteString.html
   
   Simple connection to AMQP Server (like RabbitMQ).

   Based on python AMQP Client Library
   from Barry Pederson.
   (4) http://barryp.org/software/py-amqplib/
   
   Example console output with RabbitMQ server:
  
   <<<AMQP Reader>>>
   FrameType: 1
   Channel: 0
   Size: 283
   Ch: CE
   #[0 a 0 a] method_name=Connection.start
   *********************************************** -}
module Network.WithByteString.ByteStringClient where

import Network
import System.IO

import Data.Word
import Data.Binary
import Data.Binary.Get as BinaryGet
import Data.Binary.Put as BinaryPut

import Data.ByteString as Eager (unpack, pack)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LazyC
import Data.ByteString.Lazy as Lazy (ByteString, unpack)

import Text.Printf

server = "127.0.0.1"
port = 5672

amqpHeadA = "AMQP"
amqpHeadB = 0x01010901

type Octet = Word8

data AMQPData = AMQPData {
      amqpHeaderA :: [Octet],
      amqpHeaderB :: Word32
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
      payload :: ByteString,
      ch :: Octet
}

{- *********************************************************
   Method Name Map for AMQP methods by their
   according method signature.
   ********************************************************* -}
methodNameMap :: (Integer, Integer) -> String
methodNameMap    (10, 10) = "Connection.start"
methodNameMap    (10, 11) = "Connection.start_ok"
methodNameMap    (10, 20) = "Connection.secure"
methodNameMap    (10, 21) = "Connection.secure_ok"
methodNameMap    (10, 30) = "Connection.tune"
methodNameMap    (10, 31) = "Connection.tune_ok"
methodNameMap    (10, 40) = "Connection.open"
methodNameMap    (10, 41) = "Connection.open_ok"
methodNameMap    (10, 50) = "Connection.redirect"
methodNameMap    (10, 60) = "Connection.close"
methodNameMap    (10, 61) = "Connection.close_ok"
methodNameMap    (20, 10) = "Channel.open"
methodNameMap    (20, 11) = "Channel.open_ok"
methodNameMap    (20, 20) = "Channel.flow"
methodNameMap    (20, 21) = "Channel.flow_ok"
methodNameMap    (20, 30) = "Channel.alert"
methodNameMap    (20, 40) = "Channel.close"
methodNameMap    (20, 41) = "Channel.close_ok"
methodNameMap    (30, 10) = "Channel.access_request"
methodNameMap    (30, 11) = "Channel.access_request_ok"
methodNameMap    (40, 10) = "Channel.exchange_declare"
methodNameMap    (40, 11) = "Channel.exchange_declare_ok"
methodNameMap    (40, 20) = "Channel.exchange_delete"
methodNameMap    (40, 21) = "Channel.exchange_delete_ok"
methodNameMap    (50, 10) = "Channel.queue_declare"
methodNameMap    (50, 11) = "Channel.queue_declare_ok"
methodNameMap    (50, 20) = "Channel.queue_bind"
methodNameMap    (50, 21) = "Channel.queue_bind_ok"
methodNameMap    (50, 30) = "Channel.queue_purge"
methodNameMap    (50, 31) = "Channel.queue_purge_ok"
methodNameMap    (50, 40) = "Channel.queue_delete"
methodNameMap    (50, 41) = "Channel.queue_delete_ok"
methodNameMap    (60, 10) = "Channel.basic_qos"
methodNameMap    (60, 11) = "Channel.basic_qos_ok"
methodNameMap    (60, 20) = "Channel.basic_consume"
methodNameMap    (60, 21) = "Channel.basic_consume_ok"
methodNameMap    (60, 30) = "Channel.basic_cancel"
methodNameMap    (60, 31) = "Channel.basic_cancel_ok"
methodNameMap    (60, 40) = "Channel.basic_publish"
methodNameMap    (60, 50) = "Channel.basic_return"
methodNameMap    (60, 60) = "Channel.basic_deliver"
methodNameMap    (60, 70) = "Channel.basic_get"
methodNameMap    (60, 71) = "Channel.basic_get_ok"
methodNameMap    (60, 72) = "Channel.basic_get_empty"
methodNameMap    (60, 80) = "Channel.basic_ack"
methodNameMap    (60, 90) = "Channel.basic_reject"
methodNameMap    (60, 100) = "Channel.basic_recover"
methodNameMap    (90, 10) = "Channel.tx_select"
methodNameMap    (90, 11) = "Channel.tx_select_ok"
methodNameMap    (90, 20) = "Channel.tx_commit"
methodNameMap    (90, 21) = "Channel.tx_commit_ok"
methodNameMap    (90, 30) = "Channel.tx_rollback"
methodNameMap    (90, 31) = "Channel.tx_rollback_ok"
methodNameMap _           = "Invalid"

{- *********************************************************
     Class instances
   ********************************************************* -}
instance Show AMQPFrame where
    show amq = let frame_type = (frameType amq)
              in "<<<AMQP Reader>>>\n" ++
               printf "FrameType: %X\n" (frameType amq) ++
               printf "Channel: %X\n" (channel amq) ++
               printf "Size: %d\n" (size amq) ++
               printf "Ch: %X\n" (ch amq)
               
instance Binary AMQPFrame where
    get = do
      frameType <- getWord8
      chan <- getWord16be
      sz <- getWord32be
      bytes <- BinaryGet.getLazyByteString (fromIntegral sz)
      chw <- getWord8
      return (AMQPFrame { frameType=frameType,
                           channel=chan,
                           size=sz,
                           payload=bytes,
                           ch=chw
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
     -}
     -- *******************************************************
    put amq = do
      BinaryPut.putByteString (pack (amqpHeaderA amq))
      BinaryPut.putWord32be (amqpHeaderB amq)

amqInstance :: IO AMQPData
amqInstance = return (AMQPData { amqpHeaderA = (Eager.unpack (C.pack amqpHeadA)),
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
  LazyC.hPut h bs_amq
  hFlush h

  -- Wait for frame
  bs_reader <- LazyC.hGetContents h
  let amqFrame = decode bs_reader :: AMQPFrame
  putStrLn $ show(amqFrame)
  
  -- Extract the payload, checking the method signature
  let framePayload = (payload amqFrame)
      methodSig = (take 4 (LazyC.unpack framePayload))

  -- Get the method signature values, we expect 10,10
  putStrLn $ printf "#[%x %x %x %x] method_name="
               (methodSig !! 0) (methodSig !! 1)
               (methodSig !! 2) (methodSig !! 3)
  putStrLn $ methodNameMap (10, 10)
  t <- hGetContents h
  print t
