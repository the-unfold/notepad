{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module EventRegistrator where

import qualified Data.UUID as UUID
import qualified Data.Aeson as Aeson
import Database.PostgreSQL.Typed.Query (pgSQL)
import Control.Concurrent.STM ( atomically )
import Control.Concurrent.STM.TBChan ( tryWriteTBChan, TBChan )

import DomainEvent
import Database

insertEvent :: TBChan () -> UUID.UUID -> DomainEvent -> IO ()
insertEvent chan uuid body = do 
    runQueryWithNewConnection_ [pgSQL| INSERT INTO events (uuid, body) VALUES (${uuid}, ${Aeson.toJSON body}); |]
    atomically $ tryWriteTBChan chan ()
    pure ()
