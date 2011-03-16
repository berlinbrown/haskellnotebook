--
-- Project Euler:
-- Problem1
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, 
--	we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.
--
-- Berlin Brown
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
