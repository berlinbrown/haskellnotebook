{- ***********************************************
   File: QueueClient.hs 
   Author: Berlin Brown
   *********************************************** -}
module Tests.AMQP.TestQueueClient where

import Data.AMQP.QueueClient

libVers :: String
libVers = "Vers: " ++ ampqClientVers

connectServerTest = do
  connectSimpleServer
