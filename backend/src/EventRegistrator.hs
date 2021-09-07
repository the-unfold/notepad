{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module EventRegistrator (insertEvent') where

-- Events are created by aggregates.
-- Event registrator writes event to eventlog and notifies the event bus
-- Aggregate is responsible for consistency in some business context
-- things to think about: how to update aggregateState from the event bus

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBChan (TBChan, tryWriteTBChan)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import Control.Exception (try)
import Control.Monad (void)
import Control.Monad.Except (ExceptT (..), MonadIO (liftIO), catchError, liftEither, runExceptT, throwError, withExcept, withExceptT)
import Control.Monad.State (StateT (runStateT), get, modify)
import Control.Monad.Trans (lift)
import Data.Aeson qualified as Aeson
import Data.Set (Set, insert)
import Data.Text (Text)
import Data.UUID qualified as UUID
import Database (includesText, runQueryWithNewConnection_)
import Database.PostgreSQL.Typed (PGError)
import Database.PostgreSQL.Typed.Query (pgSQL)
import DomainEvent (DomainEvent (UserRegistered, email))

insertEvent' :: DomainEvent -> ExceptT PGError IO ()
insertEvent' body = do
  let errorHandler :: PGError -> ExceptT PGError IO ()
      errorHandler e
        | includesText "unique_event_uuid" e = pure ()
        | otherwise = throwError e
  (`catchError` errorHandler) . ExceptT . try $ do
    runQueryWithNewConnection_ [pgSQL| INSERT INTO events (body) VALUES (${Aeson.toJSON body}); |]

-- No need in notifying the Event Processor in case of constraint violation,
-- because the constraint violation means that the event is already registered
-- and the Event Processor is already notified
-- void $ atomically $ writeTChan chan ()

-- After inserting, it notifies event processor via TBChan,
-- which means "a new event was registered, go and process it."
-- Note: TBChan is just a pokemon name, nothing special.
-- insertEvent :: TBChan () -> UUID.UUID -> DomainEvent -> ExceptT PGError IO ()
-- insertEvent eventBusNotificationChan uuid body = do
--   let errorHandler :: PGError -> ExceptT PGError IO ()
--       errorHandler e
--         | includesText "unique_event_uuid" e = pure ()
--         | otherwise = throwError e
--   (`catchError` errorHandler) . ExceptT . try $ do
--     runQueryWithNewConnection_ [pgSQL| INSERT INTO events (uuid, body) VALUES (${uuid}, ${Aeson.toJSON body}); |]
--     -- No need in notifying the Event Processor in case of constraint violation,
--     -- because the constraint violation means that the event is already registered
--     -- and the Event Processor is already notified
--     void $ atomically $ tryWriteTBChan eventBusNotificationChan ()
