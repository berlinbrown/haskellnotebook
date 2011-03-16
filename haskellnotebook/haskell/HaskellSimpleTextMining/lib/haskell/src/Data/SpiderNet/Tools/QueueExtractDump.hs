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
module Main where

import Monad (liftM, when)
import System.Directory (getDirectoryContents)
import List (isPrefixOf, isSuffixOf, genericLength)
import Data.SpiderNet.Bayes
import IO
import Data.SpiderNet.DocumentInfo
import Data.SpiderNet.PageInfo
import Data.SpiderNet.Document
import Data.SpiderNet.DocumentRules
import Data.SpiderNet.Util

import IO
import Database.HSQL as Hsql
import Database.HSQL.SQLite3 as Hsql

import Data.SpiderNet.Util

spiderQueueDB  = "../../var/lib/spiderdb/contentdb/spider_queue.db"
sqlCreateQueue = "create table if not exists remotequeue(url)"
sqlQueueInsert = "insert into remotequeue values("
sqlSelectUrl = "select url from remotequeue where url = "

trainDir = "../../var/lib/spiderdb/train"
inputExtractContentDir = "../../var/lib/spiderdb/dump"
stopWordsDb = "../../var/lib/spiderdb/lexicon/stopwords/stopwords.tdb"

runExtractQueue :: IO ()
runExtractQueue = do
  putStrLn "Adding to remote queue from extract dump"

  stopwords <- readStopWords stopWordsDb
  -- Process only files with 'train' extension
  traininf <- readContentByExt trainDir ".train"
  -- Train inf contains a collection of all data read from the training files.
  let traininfo = buildTrainSet traininf stopwords []
  contentinf <- readContentByExt inputExtractContentDir ".extract"
  putStrLn $ "Train Set Size=" ++ (show (length traininfo))
  putStrLn $ "Content Extract Size=" ++ (show (length contentinf))
           
  -- Based on data from stopwords, traininfo, content, generate
  -- document report.
  docreport <- toDocumentInfoList stopwords traininfo contentinf
  
  conn <- Hsql.connect spiderQueueDB ReadWriteMode
  stmt <- Hsql.query conn sqlCreateQueue
  Hsql.closeStatement stmt
  mapM_ (\inf -> sqlPutDocumentInfo conn inf) docreport
  Hsql.disconnect conn
  putStrLn "Done"

--
-- Get Rows routine from David at davblog48
getRows :: Statement -> IO [[String]]
getRows stmt = do
  let fieldtypes = map (\(a,b,c) -> a) $ getFieldsTypes stmt
  rowdata <- collectRows (\s -> mapM (getFieldValue s) fieldtypes ) stmt
  return rowdata

sqlCheckExisting :: Connection -> DocumentInfo -> IO Bool
sqlCheckExisting conn info = do 
  let pageinfo = docPageInfo info
      sql = (sqlSelectUrl ++ "'" ++ (linkUrlField pageinfo) ++ "'")
  stmt <- Hsql.query conn sql
  rows <- getRows stmt
  Hsql.closeStatement stmt
  return (length rows > 0)
  
sqlPutDocumentInfo :: Connection -> DocumentInfo -> IO ()
sqlPutDocumentInfo conn info = do
  exists <- sqlCheckExisting conn info
  if not exists
     then do let pageinfo = docPageInfo info
                 sql = (sqlQueueInsert ++ "'" ++ (linkUrlField pageinfo) ++ "')")
             stmt <- Hsql.query conn sql
             Hsql.closeStatement stmt
     else return ()

main :: IO ()
main = do
  timeDiff $ runExtractQueue
