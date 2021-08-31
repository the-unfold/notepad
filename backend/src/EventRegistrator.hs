{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module EventRegistrator (insertEvent) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBChan (TBChan, tryWriteTBChan)
import Control.Monad.Except (ExceptT (..), throwError, catchError)
import Control.Exception (try)
import Control.Monad (void)
import Data.Aeson qualified as Aeson
import Data.UUID qualified as UUID
import Database (includesText, runQueryWithNewConnection_)
import Database.PostgreSQL.Typed (PGError)
import Database.PostgreSQL.Typed.Query (pgSQL)
import DomainEvent (DomainEvent)

-- After inserting, it notifies event processor via TBChan, 
-- which means "a new event was registered, go and process it."
-- Note: TBChan is just a pokemon name, nothing special.
insertEvent :: TBChan () -> UUID.UUID -> DomainEvent -> ExceptT PGError IO ()
insertEvent chan uuid body = do
    let errorHandler :: PGError -> ExceptT PGError IO ()
        errorHandler e
          | includesText "unique_event_uuid" e = pure ()
          | otherwise = throwError e
    (`catchError` errorHandler) . ExceptT . try $ do
      runQueryWithNewConnection_ [pgSQL| INSERT INTO events (uuid, body) VALUES (${uuid}, ${Aeson.toJSON body}); |]
      -- No need in notifying the Event Processor in case of constraint violation, 
      -- because the constraint violation means that the event is already registered 
      -- and the Event Processor is already notified
      void $ atomically $ tryWriteTBChan chan () 
