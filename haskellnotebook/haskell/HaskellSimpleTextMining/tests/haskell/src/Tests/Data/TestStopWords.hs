--
-- Test Stop Words

module Tests.Data.TestStopWords where

import Data.SpiderNet.Bayes
import Data.List

stopWordsDb = "../../var/lib/spiderdb/lexicon/stopwords/stopwords.tdb"

runTestStopWords = do
  putStrLn "Run Test Stop Words"
  stopwords <- readStopWords stopWordsDb
  let datatest = [ "the", "school", "is", "over", "there", "canada", "chicken" ]
      rmwords = datatest \\ stopwords
  putStrLn $ show rmwords
  putStrLn $ "Stop Word Density=" ++ 
                   show (stopWordDensity "the school is over there canada chicken dogs" stopwords)
