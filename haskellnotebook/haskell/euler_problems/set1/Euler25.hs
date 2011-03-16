--
-- Project Euler:
-- What is the first term in the Fibonacci sequence to contain 1000 digits?
-- Date: 8/21/2008
--
module Main where

import Debug.Trace

-- Fibonacci sequence.
fibFN :: Int -> Int
fibFN 0 = 0
fibFN 1 = 1
fibFN x = fibFN(x-1) + fibFN(x-2)

fibStr :: Int -> String
fibStr x = "" ++ (show $ fibFN x)

fibDigLen :: Int -> Int
fibDigLen x = length $ fibStr x

-- trace ("Euler 25 Idx: " ++ (show x))
euler25 :: Int -> Int -> Int
euler25 x max
    | (fibDigLen x) >= max = fibFN x
    | otherwise            = (
                              (euler25 (x+1) max))

--
-- Haskell Wiki Version:
-- http://www.haskell.org/haskellwiki/Euler_problems/21_to_30
-- http://www.haskell.org/haskellwiki/HaskellWiki:Copyrights
valid ( i, n ) = length ( show n ) == 1000
problem_25     = fst . head . filter valid . zip [ 1 .. ] $ fibs
    where fibs = 1 : 1 : 2 : zipWith (+) fibs ( tail fibs )

main = do
  putStrLn "Running Problem Set"
  putStrLn $ "Euler25: " ++ fibStr 12
  putStrLn $ "Euler25: " ++ (show $ fibDigLen 12)
  putStrLn $ "Euler25: " ++ (show $ euler25 0 10)
  --putStrLn $ "-->" ++ (show problem_25)
  --putStrLn $ "-->" ++ (show $ fibFN 8192)
  putStrLn "Done"

-- End of File