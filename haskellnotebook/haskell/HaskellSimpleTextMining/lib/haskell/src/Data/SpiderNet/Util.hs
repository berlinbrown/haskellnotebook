--
--
module Data.SpiderNet.Util (dbFieldList, csvFieldNames,timeDiff) where

import System.CPUTime

csvFieldNames = [ "FILE",
                  "CHARS",
                  "TOKENS",
                  "DOCDENS",
                  "STOPDENS",
                  "VALID",
                  "LINKS",
                  "BLOCKQ",
                  "DIVS",
                  "H1S",
                  "IMGS",
                  "PARAS",
                  "STRONGS",
                  "TABLES",
                  "JUNKFILE", 
                  "BAYESPROB1", 
                  "FISHPROB1",
                  "FEATPROB1",
                  "SPAMFILE",
                  "BAYESPROB2",
                  "FISHPROB2",
                  "FEATPROB2" ]

dbFieldList :: [String] -> String
dbFieldList lst = concat $ map (\x -> x ++ ",") lst

timeDiff :: IO t -> IO t
timeDiff func = do
   start <- getCPUTime
   v <- func
   end   <- getCPUTime
   let diff = (fromIntegral (end - start)) / (10^12)
   putStrLn $ "running time:" ++ (show (diff :: Double)) ++ " s"
   return v