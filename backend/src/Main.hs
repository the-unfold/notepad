
import Control.Concurrent ( newMVar )
import Control.Concurrent.STM.TBChan ( newTBChan, TBChan )
import Control.Concurrent.STM ( atomically)

import ConcurrencyHelpers
import DomainEvent
import EventProcessor
import HttpServer ( handleRequests )

main :: IO ()
main = do
    childrenMVars <- newMVar []
    chan <- atomically $ newTBChan 1
    _ <- forkChild childrenMVars $ processEventsInLoop chan
    _ <- forkChild childrenMVars $ handleRequests chan
    waitForChildren childrenMVars
    return ()
