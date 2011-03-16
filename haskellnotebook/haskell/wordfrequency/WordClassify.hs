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

type WordCat = (String, String)
type WordCatInfo = (WordCat, Int)
type WordInfo = (String, Int)

--
-- | Find word frequency given an input list using "Data.Map" utilities.
-- With (Map.empty :: Map.Map String Int), set k = String and a = Int
--    Map.empty :: Map k a
-- ****************************
-- foldl' is a strict version of foldl = foldl': (a -> b -> a) -> a -> [b] -> a
-- Also see: updmap nm key = Map.insertWith (+) key 1 nm
-- (Original code from John Goerzen's wordFreq)
-- ****************************
-- Word Category Frequency, modified version of wordFreq to 
-- handle Word Category type.
wordCatFreq :: [WordCat] -> [WordCatInfo]
wordCatFreq inlst = Map.toList $ foldl' 
                    updateMap (Map.empty :: Map.Map WordCat Int) inlst
    where updateMap freqmap wordcat = case (Map.lookup wordcat freqmap) of
                                        Nothing -> (Map.insert wordcat 1 freqmap)
                                        Just x  -> (Map.insert wordcat $! x + 1) freqmap

formatWordCat :: WordCatInfo -> String
formatWordCat tupl = frmtcat (fst tupl) ++ " " ++ (show $ snd tupl)
    where frmtcat infotupl = (fst infotupl) ++ ", " ++ (snd infotupl)

--
-- | bayes classification train, with data and category as inputs. 
trainClassify :: String -> String -> [WordCatInfo]
trainClassify content cat = let tokens = splitRegex (mkRegex "\\s*[ \t\n]+\\s*") content
                                wordcats = [ (tok, cat) | tok <- tokens] 
                        in wordCatFreq wordcats

main :: IO ()
main = do
  putStrLn "*** Content Analysis"
  let wordcatfreq = trainClassify "viagra is bad cialis is good" "bad"
  mapM_ (\x -> (putStrLn $ formatWordCat x)) wordcatfreq
  putStrLn "*** Done"

