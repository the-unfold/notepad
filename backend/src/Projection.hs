{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

module Projection where

import Control.Exception (try)
import Control.Monad.Except (ExceptT (ExceptT), MonadError, withExceptT)
import Control.Monad.State (StateT, modify)
import Data.Set qualified as Set
import Data.Text (Text)
import Database (runQueryWithNewConnection_)
import Database.PostgreSQL.Typed (PGError)
import Database.PostgreSQL.Typed.Query (pgSQL)
import DomainEvent (DomainEvent (UserRegistered, email))

-- update :: DomainEvent -> ContextAState -> ContextAState
-- update UserRegistered {email} prev =
--   prev {registeredUserEmails = Set.insert email $ registeredUserEmails prev}
-- update _ prev = prev

-- ExceptT e m ()

data EventProcessingError = EventProcessingError

class (MonadError e m) => Projection e m | m -> e where
  update :: DomainEvent -> m ()

instance Projection EventProcessingError (ExceptT EventProcessingError IO) where
  update UserRegistered {email} = do
    let transformError :: PGError -> EventProcessingError
        transformError _pgError = EventProcessingError
    -- state <- query DB
    -- update state
    -- write state
    withExceptT transformError . ExceptT . try $ runQueryWithNewConnection_ [pgSQL| INSERT INTO users (email) VALUES (${email}); |]
  update _ = pure ()

updateStatePG :: DomainEvent -> ExceptT PGError IO ()
updateStatePG UserRegistered {email} = do
  -- state <- query DB
  -- update state
  -- write state
  ExceptT . try $ runQueryWithNewConnection_ [pgSQL| INSERT INTO users (email) VALUES (${email}); |]
updateStatePG _ = pure ()

-- initProjection :: (Projection e m) => [DomainEvent] -> m ()
-- initProjection = mapM_ update

-- runProjection :: (Projection e m) => IO ()
-- runProjection = undefined