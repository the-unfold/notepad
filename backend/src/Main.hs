module Main (main) where

import CommandProcessor (runCommandProcessing)
import Control.Concurrent (newMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBChan (TBChan, newTBChan)
import Control.Concurrent.STM.TChan (TChan, newTChan)
import DomainCommand (DomainCommand)
import HttpServer (handleRequests)
import Types.WithUuid (WithUuid)
import Utils.Concurrency (forkChild, waitForChildren)

main :: IO ()
main = do
  childrenMVars <- newMVar []
  commandsChan <- atomically newTChan :: IO (TChan (WithUuid DomainCommand))
  eventsChan <- atomically $ newTBChan 1 :: IO (TBChan ())

  _threadId <- forkChild childrenMVars $ runCommandProcessing eventsChan commandsChan
  _threadId <- forkChild childrenMVars $ handleRequests commandsChan
  -- _threadId <- forkChild childrenMVars $ projection???

  waitForChildren childrenMVars
  return ()
