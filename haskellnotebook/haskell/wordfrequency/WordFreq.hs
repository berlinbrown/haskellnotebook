-- *********************************************************
{-
Copyright (c) 2007, Botnode.com (Berlin Brown)
http://www.opensource.org/licenses/bsd-license.php
-}
-- *********************************************************

module Main where

import System.Environment
import qualified Data.Map as Map
import Data.List
import Text.Regex (splitRegex, mkRegex)

--
-- | Find word frequency given an input list using "Data.Map" utilities.
-- With (Map.empty :: Map.Map String Int), set k = String and a = Int
--    Map.empty :: Map k a
-- foldl' is a strict version of foldl = foldl': (a -> b -> a) -> a -> [b] -> a
-- (Original code from John Goerzen's wordFreq)
wordFreq :: [String] -> [(String, Int)]
wordFreq inlst = Map.toList $ foldl' updateMap (Map.empty :: Map.Map String Int) inlst
    where updateMap freqmap word = case (Map.lookup word freqmap) of
                                     Nothing -> (Map.insert word 1 freqmap)
                                     Just x  -> (Map.insert word $! x + 1) freqmap

-- | Pretty print the word/count tuple and output a string.
formatWordFreq :: (String, Int) -> String
formatWordFreq tupl = fst tupl ++ " " ++ (show $ snd tupl)

freqSort (w1, c1) (w2, c2) = if c1 == c2
                             then compare w1 w2
                             else compare c2 c1

-- Given an input list of word tokens, find the word frequency and sort the values.
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
wordFreqSort :: [String] -> [(String, Int)]
wordFreqSort inlst = sortBy freqSort . wordFreq $ inlst

goodfile = "../../var/lib/spiderdb/dump/_dump_file_4.extract"
badfile = "../../var/lib/spiderdb/dump/_dump_file_2.extract"

main :: IO ()
main = do
  putStrLn "*** Content Analysis"
  content <- readFile badfile
  let tokens = splitRegex (mkRegex "\\s*[ \t\n]+\\s*") content
      wordfreq = wordFreqSort tokens
  mapM_ (\x -> (putStrLn $ formatWordFreq x)) wordfreq
  putStrLn $ "Number of tokens found: " ++ (show . length $ wordfreq)
  putStrLn "*** Done"
