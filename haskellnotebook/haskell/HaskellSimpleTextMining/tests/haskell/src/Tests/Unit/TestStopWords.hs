--
-- Test Stop Words

module Tests.Unit.TestStopWords where

import Test.HUnit

import Data.SpiderNet.Bayes
import Data.List

stopWordsDb = "../../var/lib/spiderdb/lexicon/stopwords/stopwords.tdb"

foo :: Int -> (Int, Int)
foo x = (1, x)

runTestStopWords = do
  stopwords <- readStopWords stopWordsDb
  let datatest = [ "the", "school", "is", "over", "there", "canada", "chicken" ]
      rmwords = datatest \\ stopwords
  putStrLn $ show rmwords
  putStrLn $ "Stop Word Density=" ++ 
                   show (stopWordDensity "the school is over there canada chicken dogs" stopwords)

test1 :: Test
test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))

allTests :: Test
allTests = TestList [ TestLabel "stop words" test1 ]

