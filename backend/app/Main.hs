import Database (db)
import Control.Concurrent.Async (concurrently_)

import EventProcessor
import HttpServer
import DomainEvent

main :: IO ()
main =
  do
    concurrently_ processEventsInLoop handleRequests
