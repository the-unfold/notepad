import Control.Concurrent.Async (concurrently_)

import EventProcessor
import Control.Concurrent ( newMVar )
import Control.Concurrent.STM.TBChan ( newTBChan, TBChan )
import Control.Concurrent.STM ( atomically)

import HttpServer
import DomainEvent
import ConcurrencyHelpers

main :: IO ()
main = do
    childrenMVars <- newMVar []
    chan <- atomically $ newTBChan 1
    _ <- forkChild childrenMVars $ processEventsInLoop chan
    _ <- forkChild childrenMVars $ handleRequests chan
    waitForChildren childrenMVars
    return ()
