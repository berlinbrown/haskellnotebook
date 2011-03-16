--
-- Simple Reader Monad Example
-- From the examples at http://www.alpheccar.org/en/posts/show/60
-- Based on code from:  alpheccar
--
{-
 Used Types:
 * liftM :: Monad m => (a1 -> r) -> m a1 -> m r
 * lift :: (Monad m, MonadTrans t) => m a -> t m a
 * runReaderT :: ReaderT r m a -> r -> m a
 * asks :: MonadReader r m => (r -> a) -> m a
   "Retrieves a function of the current environment"
 * local :: (r -> r) -> m a -> m a
   "Executes a computation in a modified environment."
 * withReader :: (r' -> r) -> Reader r a -> Reader r' a
-}

module Main where

import Control.Monad.Reader (asks, local, lift, runReaderT)

--
-- Simple data structure; information about
-- a word document (number of words in doc and length of doc)
data WordInfo = WordInfo { 
      numberOfWords :: Int,
      totalLen :: Int
    }
             
initState = WordInfo { 
              numberOfWords = 10,
              totalLen = 2000
            }

incWords :: Int -> WordInfo -> WordInfo
incWords x p = p { numberOfWords = (numberOfWords p) + x }
         
--
-- Print the initial state of the document data structure     
runTest1 = do curwordct <- asks numberOfWords
              lift . putStrLn $ show curwordct
           `runReaderT` initState

--
-- Run the state test.  Note that indentation
-- matters here.
runTest = do curwordct <- asks numberOfWords
             lift . putStrLn $ show curwordct
             n <- asks totalLen
             lift . putStrLn $ show n
             -- Perform change in new environment
             -- Console output should show 40
             local (incWords 30) $ do
               curwordct <- asks numberOfWords
               lift . putStrLn $ show curwordct
             -- Console output should show 10, original value
             curwordct <- asks numberOfWords
             lift . putStrLn $ show curwordct
          `runReaderT` initState

--
-- Test Monad as Container
result = Just (negate 1)
contTest = case result of
             Just a -> Just (a + 10)
             _ -> Nothing

repeatN 0 a = return ()
repeatN n a = (a n) >> repeatN (n-1) a

testRepeat  = repeatN 3 $ \i -> do
                putStrLn $ "TEST : " ++ (show i)

main :: IO ()
main = do
  putStrLn "Example of the reader monad"
  runTest1
  runTest
  putStrLn $ show contTest
  testRepeat
  putStrLn "Done"