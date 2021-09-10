{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module MasterProjection where

import Control.Exception (try)
import Control.Monad.Except (ExceptT (ExceptT))
import Data.Text (Text)
import Database (runQueryWithNewConnection_)
import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Protocol (PGError)
import DomainEvent (DomainEvent (UserRegistered, email))

insertUser :: Text -> ExceptT PGError IO ()
insertUser email = do
  ExceptT . try $ runQueryWithNewConnection_ [pgSQL| INSERT INTO users (email) VALUES (${email}); |]

updateMasterProjection :: DomainEvent -> ExceptT PGError IO ()
updateMasterProjection UserRegistered {email} = insertUser email
updateMasterProjection _ = pure ()