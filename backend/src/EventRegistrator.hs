{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module EventRegistrator where

import qualified Data.UUID as UUID
import qualified Data.Aeson as Aeson
import Database.PostgreSQL.Typed (PGError, pgConnect, pgDisconnect, pgErrorFields, useTPGDatabase)
import Database.PostgreSQL.Typed.Query (PGSimpleQuery, pgQuery, pgSQL, pgExecute)

import DomainEvent
import Database

useTPGDatabase db -- compile time connection

insertEvent :: UUID.UUID -> DomainEvent -> IO ()
insertEvent uuid body = runQueryNoTransaction_ [pgSQL| INSERT INTO events (uuid, body) VALUES (${uuid}, ${Aeson.toJSON body}); |]
