
--
-- Test Queue

module Tests.Data.TestBayesProb where 

import Data.SpiderNet.Bayes

-- Tuple of tuples
exampleDataDef :: [((String, String), Int)]
exampleDataDef = [
 (("my", "bad"), 1),          -- 1
 (("dog", "bad"), 1),         -- 2
 (("likes", "bad"), 1),       -- 3
 (("chicken", "bad"), 1),     -- 4
 (("yes", "bad"), 1),         -- 5
 (("my1", "bad"), 1),          -- 1
 (("dog1", "bad"), 1),         -- 2
 (("likes1", "bad"), 1),       -- 3
 (("chicken1", "bad"), 1),     -- 4
 (("yes1", "bad"), 1),         -- 5
 (("viagra", "bad"), 1) ]      -- 1b (bad)

printFeatureInfo :: [WordCatInfo] -> String -> String -> IO ()
printFeatureInfo exampleData feature cat = do
  putStrLn $ "Number in Category=" ++ (show $ catCount exampleData cat)
  putStrLn $ "Feature Probability=" ++ (show $ featureProb exampleData feature cat)
  putStrLn $ "Category Probability=" ++ (show $ categoryProb exampleData feature cat)
  putStrLn $ "Weighted Probability=" ++ (show $ weightedProb exampleData feature cat 1.0)

runCatProbTest = do
  putStrLn "Bayes Test: Inv Chi"
  let cat = "good"
      feature = "sss"
  --printFeatureInfo exampleDataDef feature cat
  --putStrLn $ "Fisher Probability=" ++ (show $ fisherProb exampleDataDef ["sss", "dog" ] "bad")

  let cl = (trainClassify "Nobody owns the water" "good") ++
           (trainClassify "the quick rabbit jumps fences" "good") ++
           (trainClassify "buy pharmaceuticals now" "bad") ++
           (trainClassify "make quick money at the online casino" "bad") ++
           (trainClassify "the quick brown fox jumps" "good")
  printFeatureInfo cl "fox" "good"
  printFeatureInfo cl "fox" "bad"
  let t1 = [ "pharmaceuticals", "online", "casino", "sexxxx", "sex" ]
      t2 = [ "quick", "fox", "brown", "sexxxx", "sex" ]
      t3 = [ "poo" ]
  putStrLn $ "Fisher Probability=" ++ (show $ fisherProb cl t1 "good")
  putStrLn $ "Fisher Probability=" ++ (show $ fisherProb cl t1 "bad")
  putStrLn $ "Fisher Probability=" ++ (show $ fisherProb cl t3 "good")
  putStrLn $ "Fisher Probability=" ++ (show $ fisherProb cl t3 "bad")
  putStrLn $ "Bayes Probability=" ++ (show ((bayesProb cl t3 "good" 1.0) * 1.0))
  putStrLn $ "Bayes Probability=" ++ (show ((bayesProb cl t3 "bad" 1.0) * 1.0))
           
  -- Test feature count bug
  putStrLn $ "-- Feature Count Test"
  let cl = (trainClassify "the the the the the" "good")
  -- Train classify passes
  putStrLn $ (show cl)
  putStrLn $ (show (featureCount cl "the" "good"))
  -- Train fails, should be 5 (python version also has one)
  putStrLn $ "Document density=" ++ (show (documentDensity "the the the the the"))
  putStrLn $ "Document density=" ++ (show (documentDensity "the cat went home"))
  putStrLn "Done Bayes"

runProbTests = do
  runCatProbTest