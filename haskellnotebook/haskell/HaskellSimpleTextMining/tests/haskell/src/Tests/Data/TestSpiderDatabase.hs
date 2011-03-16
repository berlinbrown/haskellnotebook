--
-- Test

module Tests.Data.TestSpiderDatabase where 

import Time
import Data.SpiderDB.Database
import Data.Binary (decodeFile, encodeFile)

exampleDB = "../../tools/misc/dbreader/example/spiderdb_7.sdb"

printURLs :: SpiderDatabase -> IO ()
printURLs db = printAll >> return ()
    where
      urls = (spiderPool db)
      printAll = mapM_ (putStrLn . show . (\u -> (urlinfo u))) urls
                     
runDatabaseTest = do
  putStrLn "Done <spider db test>"
  putStrLn "Running Spider Database Test"
  let db = initSpiderDatabase ([])      
  putStrLn $ show(db)
  encodeFile "test.sdb" (db :: SpiderDatabase)
  newdb <- decodeFile "test.sdb" :: IO SpiderDatabase
  putStrLn $ show(newdb)
           
  -- Test printing content data.
  newdb <- decodeFile exampleDB :: IO SpiderDatabase
  putStrLn $ show(newdb)
  printURLs newdb
  putStrLn "Done"
