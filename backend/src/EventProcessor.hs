{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module EventProcessor where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBChan (TBChan, readTBChan)
import Control.Monad.Except (runExceptT, withExceptT)
import Data.Aeson qualified as Aeson
import Data.Int (Int32)
import Data.UUID qualified as UUID
import Database (runQueryWithNewConnection)
import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Protocol (PGError)
import EventRegistrator (EventLogError, getEvents)
import MasterProjection (updateMasterProjection)

data IlliterateError = CantRead EventLogError | CantWrite PGError
  deriving stock (Show)

runEventProcessing :: TBChan () -> IO ()
runEventProcessing eventsChan = do
  atomically $ readTBChan eventsChan

  res <- runQueryWithNewConnection [pgSQL| SELECT event_id FROM last_processed_event |] :: IO [Int32]

  let lastProcessedEventId =
        case res of
          (eventId : _) -> eventId
          [] -> 0

  result <- runExceptT $ do
    events <- withExceptT CantRead $ getEvents lastProcessedEventId

    withExceptT CantWrite $ mapM_ updateMasterProjection events

  case result of
    Right () -> runEventProcessing eventsChan
    Left err -> print err
