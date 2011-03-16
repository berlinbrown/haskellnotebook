--
-- TestTrain
--
module Tests.Data.TestTrainBayes where

import Monad (liftM)
import System.Directory (getDirectoryContents)
import List (isPrefixOf, isSuffixOf)
import Data.SpiderNet.Bayes

trainDir = "train"
stopWordsDb = "../../var/lib/spiderdb/lexicon/stopwords/stopwords.tdb"

runTestTrainBayes :: IO ()
runTestTrainBayes = do
  putStrLn "Test Train Bayes"
  stopwords <- readStopWords stopWordsDb
  -- Process only files with 'train' extension
  files <- getDirectoryContents trainDir
  let trainfiles = filter (isSuffixOf ".train") files
      trainpaths = map (\x -> trainDir ++ "/" ++ x) trainfiles
  lst_content <- liftM (zip trainpaths) $ mapM readFile trainpaths
  -- Print a count of the training set size
  let info = buildTrainSet lst_content ["the", "when"] []
  putStrLn $ show (length info)
  putStrLn $ show (categories info)
  let phrases = 
          [ 
           "It doesnt matter about the drugs and rock and roll, sex",
           "I agree citi, global market money business business driving force",
           "ron paul likes constitution war international freedom america",
           "viagra drugs levitra bigger",
           "Movies are fun and too enjoy",
           "war america not good",
           "poop poop",
           "the"
          ]
  -- process the following input phrases
  mapM_ (\cat -> do
             putStrLn $ "---- " ++ cat ++ " ----"
             putStrLn $ "----------------------------"
             mapM_ (\phrase -> do
                      putStrLn $ "  . [" ++ phrase ++ " ]"
                      putStrLn $ "  Bayes Probability=" ++ 
                                   (show ((bayesProb info (wordTokens phrase) cat 1.0)))
                      putStrLn $ "  Fisher Probability=" ++ 
                                   (show ((fisherProb info (wordTokens phrase) cat)))
                      putStrLn $ "  Feature Probability=" ++ 
                                   (show ((contentFeatProb info (wordTokens phrase) cat)))
                      putStrLn $ "  Document density=" ++ (show (documentDensity phrase))
                      putStrLn $ "  Stop Word Density=" ++ (show (stopWordDensity phrase stopwords))
                   ) phrases
        ) (categories info)