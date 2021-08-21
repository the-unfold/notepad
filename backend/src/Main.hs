module Main where

import ConcurrencyHelpers
import Control.Concurrent (newMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBChan (newTBChan)
import EventProcessor ( processEventsInLoop )
import HttpServer (handleRequests)

main :: IO ()
main = do
  childrenMVars <- newMVar []
  chan <- atomically $ newTBChan 1
  _ <- forkChild childrenMVars $ processEventsInLoop chan
  _ <- forkChild childrenMVars $ handleRequests chan
  waitForChildren childrenMVars
  return ()
