{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module EventRegistrator (insertEvent) where

-- Events are created by aggregates.
-- Event registrator writes event to eventlog and notifies the event bus
-- Aggregate is responsible for consistency in some business context
-- things to think about: how to update aggregateState from the event bus

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBChan (TBChan, tryWriteTBChan)
import Control.Exception (try)
import Control.Monad (void)
import Control.Monad.Except (ExceptT (..), catchError, throwError)
import Control.Monad.State (StateT, get, modify)
import Control.Monad.Trans (lift)
import Data.Aeson qualified as Aeson
import Data.Set (Set, insert)
import Data.Text (Text)
import Data.UUID qualified as UUID
import Database (includesText, runQueryWithNewConnection_)
import Database.PostgreSQL.Typed (PGError)
import Database.PostgreSQL.Typed.Query (pgSQL)
import DomainEvent (DomainEvent (UserRegistered, email))

runExceptionalMonad = runStateT . runExceptT 

processCommand :: DomainCommand -> ExceptionalShitMonad
processCommand command = do
  -- eventsA <- runExceptionalMonad $ noteTakingAggregate command
  eventsA <- noteTakingAggregate command
  -- eventsB <- runExceptionalMonad aggregateB command
  -- mapM insertEvent $ eventsA <> eventsB
  fmap insertEvent eventsA

processCommandInLoop :: TChan DomainCommand -> ExeptionShitMonad
processCommandInLoop chan = do
  let command = atomically $ readTChan chan


-- After inserting, it notifies event processor via TBChan,
-- which means "a new event was registered, go and process it."
-- Note: TBChan is just a pokemon name, nothing special.
insertEvent :: TBChan () -> UUID.UUID -> DomainEvent -> ExceptT PGError IO ()
insertEvent eventBusNotificationChan uuid body = do
  let errorHandler :: PGError -> ExceptT PGError IO ()
      errorHandler e
        | includesText "unique_event_uuid" e = pure ()
        | otherwise = throwError e
  (`catchError` errorHandler) . ExceptT . try $ do
    runQueryWithNewConnection_ [pgSQL| INSERT INTO events (uuid, body) VALUES (${uuid}, ${Aeson.toJSON body}); |]
    -- No need in notifying the Event Processor in case of constraint violation,
    -- because the constraint violation means that the event is already registered
    -- and the Event Processor is already notified
    void $ atomically $ tryWriteTBChan eventBusNotificationChan ()

----- here goes note taking aggregate, which is Context A
newtype ContextAState = ContextAState {registeredUserEmails :: Set Text}

-- ErrorType
data FuckYou = FuckYou

data DomainCommand
  = RegisterUser {userEmail :: Text}
  | BringMeSomeBeer

type ExceptionalShitMonad s = ExceptT FuckYou (StateT s IO)

-- эту хуйню дёргать будет event processor
fuckContext :: DomainEvent -> ContextAState -> ContextAState
fuckContext UserRegistered {email} ContextAState {registeredUserEmails} =
  ContextAState {registeredUserEmails = insert email registeredUserEmails}

type Aggregate a = DomainCommand -> ExceptionalShitMonad a [DomainEvent]

-- Use only in event processor
updateAccordingToEvent :: DomainEvent -> ExceptionalShitMonad ContextAState ()
updateAccordingToEvent event =
  modify $ fuckContext event

noteTakingAggregate :: Aggregate ContextAState
noteTakingAggregate RegisterUser {userEmail} = do
  -- Note: no state updates here. only from the EventBus/EventLog.
  ContextAState {registeredUserEmails} <- get
  -- validate:
  if userEmail `elem` registeredUserEmails
    then throwError FuckYou
    else pure [UserRegistered {email = userEmail}]
-- if esists, then reject, otherwise approve and create a corresponding event

noteTakingAggregate _ = pure [] -- not handling this command
