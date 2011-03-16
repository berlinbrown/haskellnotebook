-- *********************************************************
{-
File: Document.hs

Copyright (c) 2007, Botnode.com (Berlin Brown)
http://www.opensource.org/licenses/bsd-license.php

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice, 
    this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, 
    this list of conditions and the following disclaimer in the documentation 
    and/or other materials provided with the distribution.
    * Neither the name of the Newspiritcompany.com (Berlin Brown) nor 
    the names of its contributors may be used to endorse or promote 
    products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Description:

Document utilities

Also see:
 (1) http://www.haskell.org/ghc/docs/latest/html/libraries/containers/Data-Map.html

-}
-- *********************************************************

module Data.SpiderNet.Document (readContentByExt, toDocumentInfoList,
                                readInfoContentFile) where

import Monad (liftM)
import System.Directory (getDirectoryContents)
import Data.Char
import Data.List
import IO (try)
import List (isPrefixOf, isSuffixOf)
import Text.Regex (splitRegex, mkRegex)

import Data.SpiderNet.PageInfo
import Data.SpiderNet.DocumentInfo
import Data.SpiderNet.DocumentRules
import Data.SpiderNet.Bayes

type ContentFileInfo = (String, String)

readContentByExt :: String -> String -> IO [ContentFileInfo]
readContentByExt filepath ext = do 
  files <- getDirectoryContents filepath        
  let trainfiles = filter (isSuffixOf ext) files
      trainpaths = map (\x -> filepath ++ "/" ++ x) trainfiles
  lst_content <- liftM (zip trainpaths) $ mapM readFile trainpaths
  return lst_content

--
-- The info content file contains html document information.
-- It may not exist but should, also contains URL info.
readInfoContentFile :: String -> IO PageURLFieldInfo
readInfoContentFile extr_file = do
  let extr_n = (length ".extract")
      extr_path = take ((length extr_file) - extr_n) extr_file
      info_file = extr_path ++ ".info"
  -- Extract the file, in CSV format.
  -- TYPE::|URL::|a::|b::|blockquote::|div::|h1::|h2::|i::|img::|p::|span::|strong::|table
  -- (+1)   0     1   X2   3           4     5    X6   X7   8     9   X10   11      12    
  csvtry <- try $ readFile info_file
  -- Handle error
  info <- case csvtry of
            Left _ -> return defaultPageFieldInfo
            Right csv -> do let csv_lst = splitRegex (mkRegex "\\s*(::\\|)+\\s*") csv
                            return PageURLFieldInfo {
                                         linkUrlField = (csv_lst !! 0),
                                         aUrlField = read (csv_lst !! 1) :: Integer,
                                         blockquoteUrlField = read (csv_lst !! 3) :: Integer,
                                         divUrlField = read (csv_lst !! 4) :: Integer,
                                         h1UrlField = read (csv_lst !! 5) :: Integer,
                                         imgUrlField = read (csv_lst !! 6) :: Integer,
                                         pUrlField = read (csv_lst !! 7) :: Integer,
                                         strongUrlField = read (csv_lst !! 8) :: Integer,
                                         tableUrlField = 0
                                       }
  return info

getCatTrainInfo :: [WordCatInfo] -> String -> [String] -> IO DocTrainInfo
getCatTrainInfo traininfo cat wordtokens = do
  let bp = bayesProb traininfo wordtokens cat 1.0
      fp = fisherProb traininfo wordtokens cat
      cfp = contentFeatProb traininfo wordtokens cat
  return DocTrainInfo {
                    trainCatName = cat,
                    trainBayesProb = bp,
                    trainFisherProb = fp,
                    trainFeatureProb = cfp
                  }

toDocumentInfoList :: [String] -> [WordCatInfo]  -> [(String, String)] -> IO [DocumentInfo]
toDocumentInfoList stopwords traininfo contentinf =
    mapM (\contentinfo -> do
            let content = snd contentinfo
                contentname = fst contentinfo
                wordtokens = inputDocumentTokens content stopwords
                docdens = documentDensity content
                stopdens = stopWordDensity content stopwords
            readpageinfo <- readInfoContentFile contentname
            doctrain <- mapM (\c -> getCatTrainInfo traininfo c wordtokens) (categories traininfo)
            let rules_lst = populateRulesInput (genericLength wordtokens) docdens stopdens readpageinfo
            return DocumentInfo {
                         docName = contentname,
                         docCharLen = genericLength content,
                         docTokenLen = genericLength wordtokens, 
                         docWordDensity = docdens,
                         docStopWordDensity = stopdens,
                         docTrainInfo = doctrain,
                         docPageInfo = readpageinfo,
                         docIsValidPage = checkDocRules rules_lst
                       }
         ) contentinf

-- End of File