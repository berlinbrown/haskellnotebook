--
-- Project Euler:
-- Problem3
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 317584931803?
-- References:
-- (1) http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
-- Berlin Brown
-- 
module Main where

isPrimeBrute :: Integer -> Bool
isPrimeBrute 2           = True
isPrimeBrute n 
    | n < 2              = False
    | ((n `mod` 2) == 0) = False
    | otherwise          = isPrimeBrute(n - 1)

primes = sieve [2..]
    where
      sieve (p:x) = p : sieve [ n | n <- x, n `mod` p > 0 ]

main = do
  putStrLn "Running Problem Set"
  putStrLn $ "Primes: " ++ show (isPrimeBrute 1000)
  putStrLn $ "Primes: " ++ show (take 5 primes)
  putStrLn "Done"
