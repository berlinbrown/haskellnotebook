{- ***********************************************
   File: QueueClient.hs 
   Author: Berlin Brown
   *********************************************** -}
module Main where

import Test.HUnit

import qualified Tests.Unit.TestExample as Example
import qualified Tests.Unit.TestStopWords as Stopwords

import Time
import Data.Time.Clock.POSIX

main = do
  ct <- getClockTime
  pt <- getPOSIXTime
  putStrLn $ "Running Tests"
  putStrLn $ "At: " ++ (show ct) ++ " t:" ++ (show (round pt))
  runTestTT Example.allTests
  runTestTT Stopwords.allTests
  putStrLn "Done"

