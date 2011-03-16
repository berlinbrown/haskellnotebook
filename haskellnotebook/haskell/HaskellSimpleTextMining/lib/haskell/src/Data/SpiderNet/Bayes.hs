-- *********************************************************
{-
File: Bayes.hs

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

Note: I use the term feature and token interchangably, most documents
when talking about bayesian filters use the term feature.

Also see:
 (1) http://www.haskell.org/ghc/docs/latest/html/libraries/containers/Data-Map.html

-}
-- *********************************************************

module Data.SpiderNet.Bayes 
    (WordCat, WordCatInfo, WordInfo, 
     documentDensity, inputDocumentTokens,
     wordFreq, wordCatFreq, formatWordFreq, buildTrainSet, wordTokensClean,
     formatWordCat, wordFreqSort, trainClassify, contentFeatProb,formatTrainInfo,
     tokensCat, tokensByFeature, catCount, wordTokens, readStopWords,
     categories, featureCount, featureProb, bayesProb, stopWordDensity,
     categoryProb, weightedProb, invChi2, fisherProb) where

import System.Environment
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Text.Printf
import Data.Char
import Text.Regex (splitRegex, mkRegex)

import Data.SpiderNet.DocumentInfo
import Data.SpiderNet.PageInfo

-- Max number of word tokens to use as input to the training model.
maxTokensDocTrain = 4000

type WordCat = (String, String)
type WordCatInfo = (WordCat, Int)
type WordInfo = (String, Int)

wordTokens :: String -> [String]
wordTokens content = tokens
    where maxwordlen = 100
          lowercase str = map toLower str
          alltokens = splitRegex (mkRegex "\\s*[ \t\n]+\\s*") (lowercase content)
          tokens = filter (\x -> length x > 1 && length x < maxwordlen) alltokens

--
-- | Tokenize document, clean and remove any stop words.
wordTokensClean :: String -> [String] -> [String]
wordTokensClean content stopwords = (wordTokens content) \\ stopwords

--
-- | Find word frequency given an input list using "Data.Map" utilities.
-- With (Map.empty :: Map.Map String Int), set k = String and a = Int
--    Map.empty :: Map k a
-- foldl' is a strict version of foldl = foldl': (a -> b -> a) -> a -> [b] -> a
-- Also see: updmap nm key = Map.insertWith (+) key 1 nm
-- (Original code from John Goerzen's wordFreq)
wordFreq :: [String] -> [WordInfo]
wordFreq inlst = Map.toList $ foldl' updateMap (Map.empty :: Map.Map String Int) inlst
    where updateMap freqmap word = case (Map.lookup word freqmap) of
                                         Nothing -> (Map.insert word 1 freqmap)
                                         Just x  -> (Map.insert word $! x + 1) freqmap

--
-- | Word Category Frequency, modified version of wordFreq to 
-- handle Word Category type.
wordCatFreq :: [WordCat] -> [WordCatInfo]
wordCatFreq inlst = Map.toList $ foldl' 
                    updateMap (Map.empty :: Map.Map WordCat Int) inlst
    where updateMap freqmap wordcat = case (Map.lookup wordcat freqmap) of
                                        Nothing -> (Map.insert wordcat 1 freqmap)
                                        Just x  -> (Map.insert wordcat $! x + 1) freqmap

-- | Pretty print the word/count tuple and output a string.
formatWordFreq :: WordInfo -> String
formatWordFreq tupl = fst tupl ++ " " ++ (show $ snd tupl)

formatWordCat :: WordCatInfo -> String
formatWordCat tupl = frmtcat (fst tupl) ++ " " ++ (show $ snd tupl)
    where frmtcat infotupl = (fst infotupl) ++ ", " ++ (snd infotupl)

freqSort (w1, c1) (w2, c2) = if c1 == c2
                             then compare w1 w2
                             else compare c2 c1

-- Given an input list of word tokens, find the word frequency and sort the values.
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
wordFreqSort :: [String] -> [(String, Int)]
wordFreqSort inlst = sortBy freqSort . wordFreq $ inlst

--
-- | bayes classification train 
-- @see trainClassifyClean, clean filters top words
trainClassify :: String -> String -> [WordCatInfo]
trainClassify content cat = let tokens = (wordTokens content)
                                wordcats = [(tok, cat) | tok <- tokens] 
                            in wordCatFreq wordcats

--
-- | Public function for filtering the raw input document and converting
-- the data into a word token list.
-- 1/24/2008: remove stop words.
-- 1/24/2008: Added take function call to set a max number of words to process.
inputDocumentTokens :: String -> [String] -> [String]
inputDocumentTokens content stopwords = take maxTokensDocTrain ((wordTokens content) \\ stopwords)
                         
--
-- | classification train and eliminate stop words from the training set.
-- == Modifications ==
-- 1/24/2008: Added function for stop word support
trainClassifyClean :: String -> String -> [String] -> [WordCatInfo]
trainClassifyClean content cat stop_words = let tokens = ((wordTokens content) \\ stop_words)
                                                wordcats = [(tok, cat) | tok <- tokens] 
                                            in wordCatFreq wordcats

--
-- | Read stop words from the file, return a list of strings.
readStopWords :: FilePath -> IO [String]
readStopWords path = readFile path >>= (\input -> return $ wordTokens input)

--
-- Build a set of training data from input content/category information; include
-- support for ignoring stop words.
--
-- Inputs:
-- List of content/category tuple rows
-- List of WordCatInfo rows
buildTrainSet :: [(String, String)] -> [String] -> [WordCatInfo] -> [WordCatInfo]
buildTrainSet []     stop_words info = info
buildTrainSet (x:xs) stop_words info = info ++ buildTrainSet xs stop_words (trainClassifyClean (snd x) (fst x) stop_words)

--
-- | Return only the tokens in a category.
tokensCat :: [WordCatInfo] -> String -> [WordCatInfo]
tokensCat tokens cat = let getTokCat row = snd (fst row)
                           tokbycat = filter (\x -> ((getTokCat x) == cat)) tokens
                       in tokbycat

tokensByFeature :: [WordCatInfo] -> String -> String -> [WordCatInfo]
tokensByFeature tokens tok cat = filter (\x -> ((fst x) == (tok, cat))) tokens

--
-- | Count of number of features in a particular category
-- Extract the first tuple to get the WordCat type and then the
-- second tuple to get the category.
catCount :: [WordCatInfo] -> String -> Integer
catCount tokens cat = genericLength $ tokensCat tokens cat

-- Find the distinct categories
categories :: [WordCatInfo] -> [String]
categories tokens = let getTokCat row = snd (fst row)                     
                        allcats = Set.toList . Set.fromList $ [ getTokCat x | x <- tokens ]
                    in allcats

featureCount :: [WordCatInfo] -> String -> String -> Integer
featureCount tokens tok cat = genericLength $ tokensByFeature tokens tok cat

totalCount :: [WordCatInfo] -> Integer
totalCount features = genericLength features

--
-- | Feature probality, count in this category over total in category
featureProb :: [WordCatInfo] -> String -> String -> Double
featureProb features tok cat = let fct = featureCount features tok cat
                                   catct = catCount features cat
                                   res | (catct == 0) = 0
                                       | (fct == 0)   = 0
                                       | otherwise    = (fromIntegral fct) / (fromIntegral catct)
                               in res

--
-- | Calcuate the category probability
categoryProb :: [WordCatInfo] -> String -> String -> Double
categoryProb features tok cat = res
    where initfprob = featureProb features tok cat
          freqsum | initfprob == 0 = 0
                  | otherwise      = sum [ (featureProb features tok x) | x <- categories features ]
          res     | freqsum == 0   = 0
                  | otherwise      = initfprob / freqsum

weightedProb :: [WordCatInfo] -> String -> String -> Double -> Double
weightedProb features tok cat weight = ((weight*ap)+(totals*initprob))/(weight+totals)
    where initprob = categoryProb features tok cat
          ap = 0.5
          totals = fromIntegral $ sum [ (featureCount features tok x) | x <- categories features ]

--
-- Weight Probablity function with probability function argument.
weightedProbFunc :: ([WordCatInfo] -> String -> String -> Double) -> [WordCatInfo] -> String -> String -> Double -> Double
weightedProbFunc funcProb features tok cat weight = ((weight*ap)+(totals*initprob))/(weight+totals)
    where initprob = funcProb features tok cat
          ap = 0.5
          totals = fromIntegral $ sum [ (featureCount features tok x) | x <- categories features ]

-- Inverted Chi2 formula
{-
Prev Version:
invChi2 :: Double -> Double -> Double
invChi2 chi df    = minimum([snd newsum, 1.0])
    where m       = chi / 2.0
          initsum = exp (-m)
          inittrm = exp (-m)
          maxrg   = fromIntegral (floor (df / 2.0)) :: Double
          -- Return a tuple with current sum and term, given these inputs
          newsum  = foldl (\(trm, sm) elm -> ((trm * (m/elm)), sm+trm)) 
                    (inittrm, initsum) [1..1]
-}
invChi2 :: Double -> Double -> Double
invChi2 chi df = min (sum termsList + exp (negate m)) 1.0
    where m = chi / 2.0
          terms :: Int -> Double
          terms 0 = exp (negate m)
          terms i = terms (i - 1) * m / fromIntegral i
          termsList = map terms [1..truncate df `div` 2 - 1]

fisherProb :: [WordCatInfo] -> [String] -> String -> Double
fisherProb features tokens cat = invchi
    where initp = 1.0
          weight = 1.0
          p = foldl (\prb f -> (prb * (weightedProb features f cat weight))) initp tokens
          fscore = (negate 2) * (log p)
          invchi = invChi2 fscore ((genericLength tokens) * 2)

--
-- Calculate all probabilities in the content document
-- Used with bayes probability.
contentProb :: [WordCatInfo] -> [String] -> String -> Double -> Double
contentProb features tokens cat weight = p
    where initp = 1.0
          p = foldl (\prb f -> (prb * (weightedProbFunc featureProb features f cat weight))) initp tokens

bayesAvgUtil xs = sum xs / fromIntegral (length xs)

--
-- Public function, calculate feature probablity for each feature
contentFeatProb :: [WordCatInfo] -> [String] -> String -> Double
contentFeatProb features tokens cat = bayesAvgUtil p
    where p = map (\f -> featureProb features f cat) tokens

--
-- Bayes probability calculaion
bayesProb :: [WordCatInfo] -> [String] -> String -> Double -> Double
bayesProb features tokens cat weight = p
    where
      catprob = fromIntegral (catCount features cat) / fromIntegral (totalCount features)
      prob = contentProb features tokens cat weight
      p = prob * catprob
          

--
-- Document Word Density, how many terms are used through out the document.
documentDensity :: String -> Double
documentDensity content = unqct / ct
    where tokens = wordTokens content
          ct = fromIntegral (length tokens)
          unqct = fromIntegral (length (Set.toList . Set.fromList $ tokens))
                            
--
-- | What percentage of the document is made up of stop words.
stopWordDensity :: String -> [String] -> Double
stopWordDensity content stop_words = (ct - nostop_ct) / ct
    where tokens = wordTokens content
          ct = fromIntegral (length tokens)
          nostop_ct = fromIntegral $ length (tokens \\ stop_words)

-- End of File