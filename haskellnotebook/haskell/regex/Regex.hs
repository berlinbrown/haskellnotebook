--
-- Simple Regex Example

module Main where

import Data.Char
import Text.Regex (splitRegex, mkRegex)

wordTokens :: String -> [String]
wordTokens content = tokens
    where maxwordlen = 100
          lowercase str = map toLower str
          alltokens = splitRegex (mkRegex "\\s*[ \t\n]+\\s*") (lowercase content)
          tokens = filter (\x -> length x > 1 && length x < maxwordlen) alltokens

main :: IO ()
main = do
  putStrLn "Running Regex Example"

  let test1 = "     My         dog      \thas chicken     "
      tokens = wordTokens test1
  putStrLn test1
  putStrLn $ show (tokens)
  putStrLn "Done"