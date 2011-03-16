--
-- Berlin Brown
-- BotWords.hs
--

module Main where

import Monad
import System.Environment
import System.Exit
import Control.Monad.Error
import Data.IORef
import Text.ParserCombinators.Parsec hiding (spaces)
import IO hiding (try)

type Token = String

simpleDocumentName :: FilePath
simpleDocumentName = "usconst.txt"

splitWords :: String -> [Token]
splitWords st = split (dropSpace st)

dropWord :: String -> String
dropWord [] = []
dropWord (x:xs)
         | elem x whitespace  = dropSpace (xs)
         | otherwise          = (x:xs)

parseTextDocument fname = readFile fname

runParseDocument :: IO ()
runParseDocument = do
  doc <- parseTextDocument simpleDocumentName
  putStrLn doc
  return ()

main = do
  putStrLn "Running BotWords"
  runParseDocument
  putStrLn "Done"
