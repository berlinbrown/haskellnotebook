--
--
module Data.SpiderNet.DocumentInfo (DocumentInfo(DocumentInfo), trainCatName, trainBayesProb, trainFisherProb,trainFeatureProb, 
                                    DocTrainInfo(DocTrainInfo), docName, docCharLen, docTokenLen, formatTrainInfo,
                                    docWordDensity, docTrainInfo, docStopWordDensity, docPageInfo, docIsValidPage) where
import System.Environment
import Text.Printf
import Data.Char
import Data.SpiderNet.PageInfo

data DocTrainInfo = DocTrainInfo {
      trainCatName :: String, 
      trainBayesProb :: Double,
      trainFisherProb :: Double,
      trainFeatureProb :: Double
}
data DocumentInfo = DocumentInfo {
      docName :: String,
      docCharLen :: Integer,
      docTokenLen :: Integer,
      docWordDensity :: Double,
      docStopWordDensity :: Double,
      docTrainInfo :: [DocTrainInfo],
      docPageInfo :: PageURLFieldInfo, 
      docIsValidPage :: Bool
}

formatTrainInfo :: [DocTrainInfo] -> String
formatTrainInfo []     = ""
formatTrainInfo (x:xs) = (show x) ++ formatTrainInfo xs

instance Show DocumentInfo where
    show info = (printf "%s,%d,%d,%f,%f,%s,"
                 (docName info) (docCharLen info) (docTokenLen info)
                 (docWordDensity info) (docStopWordDensity info) 
                 (show (docIsValidPage info)))

instance Show DocTrainInfo where
    show info = (printf "%s,%f,%f,%f,"
                 (trainCatName info) (trainBayesProb info) (trainFisherProb info)
                 (trainFeatureProb info))

-- End of File