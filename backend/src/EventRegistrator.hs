{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module EventRegistrator (insertEvent) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBChan (TBChan, tryWriteTBChan)
import Control.Monad (void)
import Data.Aeson qualified as Aeson
import Data.UUID qualified as UUID
import Database (runQueryWithNewConnection_)
import Database.PostgreSQL.Typed.Query (pgSQL)
import DomainEvent (DomainEvent)

-- Always successful, TODO: don't fail on duplicate UUIDs, instead, return OK.
-- After inserting, it notifies event processor via TBChan, which means "a new event was registered, go and process it."
insertEvent :: TBChan () -> UUID.UUID -> DomainEvent -> IO ()
insertEvent chan uuid body = do
  runQueryWithNewConnection_ [pgSQL| INSERT INTO events (uuid, body) VALUES (${uuid}, ${Aeson.toJSON body}); |]
  void $ atomically $ tryWriteTBChan chan ()
