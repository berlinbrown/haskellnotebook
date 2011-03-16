{- ***********************************************
   File: QueueClient.hs 
   Author: Berlin Brown
   *********************************************** -}
module Main where

import Tests.AMQP.TestQueueClient
import Tests.Data.TestQueue
import Tests.Data.TestSpiderDatabase
import Tests.Data.TestBayesInvChi
import Tests.Data.TestBayesProb
import Tests.Data.TestTrainBayes
import Tests.Data.TestStopWords
import Tests.Data.TestBasicHSQL

import Time
import Data.Time.Clock.POSIX

main = do
  ct <- getClockTime
  pt <- getPOSIXTime
  putStrLn $ "Running Tests"
  putStrLn $ "At: " ++ (show ct) ++ " t:" ++ (show (round pt))
  --putStrLn $ "AMQP Queue Client=" ++ libVers
  --connectServerTest
  --putStrLn "Test Build Queue (2)"
  --runQueueTest
  --putStrLn "Test Spider Database (3)"
  --runDatabaseTest
  --runBayesTest
  --runProbTests
  --runTestTrainBayes
  --runTestStopWords
  runTestBasicHSQL
  putStrLn "Done"

