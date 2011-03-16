{- ***********************************************
   File: QueueClient.hs 
   Author: Berlin Brown
   *********************************************** -}
module Main where

import Network.Simple.SimpleClient

main = do
  putStrLn "Running TestMain"
  connectSimpleServer
  putStrLn "Done"
