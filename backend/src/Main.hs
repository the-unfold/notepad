module Main (main) where

-- import EventProcessor (processEventsInLoop)
-- import EventProcessor (processEventsInLoop)
import CommandProcessor (ContextAState (ContextAState), cuntProcessCommand, processCommand, registeredUserEmails, runCommandProcessing, runExceptStateT, userNoteIds)
import Control.Concurrent (newMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBChan (TBChan, newTBChan)
import Control.Concurrent.STM.TChan (TChan, newTChan)
import Control.Monad (void)
import Data.Map qualified as Map
import Data.Set qualified as Set
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
  let initialState = ContextAState {registeredUserEmails = Set.empty, userNoteIds = Map.empty}

  _threadId <- forkChild childrenMVars $ runCommandProcessing initialState eventsChan commandsChan

  _threadId <- forkChild childrenMVars $ handleRequests commandsChan
  waitForChildren childrenMVars
  return ()
