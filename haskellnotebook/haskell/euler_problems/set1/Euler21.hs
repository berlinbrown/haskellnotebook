--
-- Project Euler:
-- What is the first term in the Fibonacci sequence to contain 1000 digits?
--
module Main where

import Ix

dataRangeSet = range(1, 999)
findMultiples = filter (\x -> ((x `mod` 5) == 0) || ((x `mod` 3) == 0)) dataRangeSet

sumTotal :: Integer
sumTotal = let res = findMultiples
                     in sum(res)

main = do
  putStrLn "Running Problem Set"
  putStrLn $ "Total: " ++ (show sumTotal)
  putStrLn "Done"
