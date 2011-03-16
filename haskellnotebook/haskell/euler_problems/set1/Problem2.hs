--
-- Project Euler:
-- Problem2
--
-- Each new term in the Fibonacci sequence is generated 
-- by adding the previous two terms. By starting with 1 and 2, 
-- the first 10 terms will be:
-- 
-- 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
-- 
-- Find the sum of all the even-valued terms in the sequence which do not exceed one million.
--
-- Berlin Brown
--
module Main where

-- Useful list notes: [ n + x + y | x <- [2..] | y <- [1..] ]

fibFN :: Integer -> Integer
fibFN 0 = 0
fibFN 1 = 1
fibFN x = fibFN(x-1) + fibFN(x-2)

fibLst :: [Integer] -> [Integer]
fibLst lst = map fibFN lst

fibSeq :: Integer -> [Integer]
fibSeq n 
    | n > 32    = fibLst [2..(32)]
    | otherwise = fibLst [2..(n+1)]

--
-- Note a standard sumAll, used for project euler,
-- Only sum even values.
sumAllFib :: [Integer] -> Integer
sumAllFib []      =  0
sumAllFib (x:xs)  
    | even(x) && (x < 1000000) = x + sumAllFib xs                 
    | otherwise                = 0 + sumAllFib xs

main = do
  putStrLn "Running Problem Set"
  putStrLn $ "Result fib: " ++ (show (fibFN 34))
  putStrLn $ "Result: " ++ (show (fibSeq 29))
  putStrLn $ "Result: " ++ (show (sumAllFib (fibSeq 29)))
  putStrLn "Done"
