{- ***********************************************
   File: QueueClient.hs 
   Author: Berlin Brown
   *********************************************** -}
module Main where

import Network.WithByteString.ByteStringClient

main = do
  putStrLn "Running TestMain"
  connectSimpleServer
  putStrLn "Done"
