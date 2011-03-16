--
-- Test Queue

module Tests.Data.TestQueue where 

import Time
import Data.Time.Clock.POSIX
import Data.SpiderQueue.Queue 
import Data.Binary (encodeFile, decodeFile)

runQueueTest = do
  putStrLn "Build Simple Queue"
  pt <- getPOSIXTime
  let row = initQueueObject "12354343" (round pt)
      q   = initSpiderQueue (row : [])      
  putStrLn $ show(q) ++ show(row)
  encodeFile "test.db" (q :: SpiderQueue)
  -- Now read the database and print
  newq <- decodeFile "test.db" :: IO SpiderQueue
  putStrLn $ show(newq)
  putStrLn "Done"
