module Main (main) where

-- import EventProcessor (processEventsInLoop)
import CommandProcessor (processCommandsInLoop)
import Control.Concurrent (newMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBChan (TBChan, newTBChan)
import Control.Concurrent.STM.TChan (TChan, newTChan)
import DomainCommand (DomainCommand)
import HttpServer (handleRequests)
import Types.WithUuid (WithUuid)
import Utils.Concurrency

main :: IO ()
main = do
  childrenMVars <- newMVar []
  commandsChan <- atomically newTChan :: IO (TChan (WithUuid DomainCommand))
  eventsChan <- atomically $ newTBChan 1 :: IO (TBChan ())
  -- _ <- forkChild childrenMVars $ processEventsInLoop chan
  _ <- forkChild childrenMVars $ processCommandsInLoop commandsChan eventsChan
  _ <- forkChild childrenMVars $ handleRequests commandsChan
  waitForChildren childrenMVars
  return ()
