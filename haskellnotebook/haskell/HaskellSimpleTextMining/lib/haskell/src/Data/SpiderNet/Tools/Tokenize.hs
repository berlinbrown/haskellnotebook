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

module Main where

import System
import System.Console.GetOpt
import Data.Maybe( fromMaybe )

import qualified Data.Set as Set
import Data.List
import Data.SpiderNet.Bayes


versTokenize = "0.0"

data Options = Options  {
      -- Usage: IO String >>= String -> IO ()
      optInput  :: IO String,
      optOutput :: String -> IO ()
    }

--
-- Given an unclean content set; tolower, filter by length, get unique tokens,
-- tokenize, join the list back together with a token on each line.
-- @see  intersperse ',' "abcde" == "a,b,c,d,e"
tokenizeInput :: String -> IO String
tokenizeInput content = return $ concat . intersperse "\n" $ unify
    where tokens = wordTokens content
          unify = Set.toList . Set.fromList $ tokens

printOutput :: String -> IO ()
printOutput inval = do
  putStrLn inval

defaultOptions :: Options
defaultOptions = Options {
                   optInput  = return "",
                   optOutput = printOutput
                 }

options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['v'] ["version"] (NoArg showVersion)         "show version number",
    Option ['i'] ["input"]   (ReqArg readInput "FILE")   "input file to read",
    Option ['o'] ["output"]  (ReqArg writeOutput "FILE") "output file"
  ]

readInput arg opt = return opt { 
                      optInput = readFile arg 
                    }
writeOutput arg opt = return opt { 
                        optOutput = writeFile arg 
                      }
showVersion _ = do
  putStrLn $ "Commandline example " ++ versTokenize
  exitWith ExitSuccess

main :: IO ()
main = do
  args <- getArgs
  let (actions, nonOpts, msgs) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { 
         optInput = input,
         optOutput = output 
       } = opts
  -- infix: input: (IO String / m a), output: (String -> IO () / a -> m b)
  input >>= tokenizeInput >>= output