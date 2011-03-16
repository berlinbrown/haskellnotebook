{- ***********************************************
   File: SimpleClient.hs
   See
   (1) http://www.haskell.org/ghc/docs/6.8.2/html/libraries/network/Network.html
   (2) http://hackage.haskell.org/packages/archive/binary/0.4.1/doc/html/Data-Binary.html
   *********************************************** -}
module Network.Simple.SimpleClient where

import Network
import System.IO

server = "127.0.0.1"
port = 8080

connectSimpleServer = do
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  t <- hGetContents h
  print t
