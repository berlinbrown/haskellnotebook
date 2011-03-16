--
-- Test Queue

module Tests.Data.TestBayesInvChi where 

import Data.SpiderNet.Bayes

exampleData :: [(Double, Double)]
exampleData = [
    (4.3, 6),
    (2.2, 60),
    (60, 2.2),
    (0.3, 4),
    (32.123, 20),
    (12.4, 5),
    (0.04, 3),
    (0, 0),
    (1, 1)]

-- Inverted Chi2 formula
invChi :: Double -> Double -> Double
invChi chi df = minimum([snd newsum, 1.0])
    where m = chi / 2.0
          initsum = exp (-m)
          trm = exp (-m)
          maxrg = fromIntegral (floor (df / 2.0)) :: Double
          -- Return a tuple with current sum and term, given these inputs
          newsum = foldl (\(trm,sm) elm -> ((trm*(m/elm)), sm+trm)) 
                   (trm,initsum) [1..maxrg]

runBayesTest = do
  putStrLn "Bayes Test: Inv Chi"
  mapM_ (\x -> putStrLn $ (show x) ++ " res=" ++ (show (invChi (fst x) (snd x)))) exampleData
  putStrLn "Done Bayes"