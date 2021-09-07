{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CommandProcessor (processCommandsInLoop, runCommandProcessing, cuntProcessCommand, processCommand, runExceptStateT, ContextAState (..)) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBChan (TBChan, writeTBChan)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), liftEither, mapExceptT, runExceptT, withExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (MonadState, StateT (runStateT), gets, modify)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Int (Int32)
import Data.Map (Map)
import Data.Set (Set, insert)
import Data.Text (Text)
import Database (runQueryWithNewConnection_)
import Database.PostgreSQL.Typed (PGError)
import Database.PostgreSQL.Typed.Query (pgSQL)
import DomainCommand (DomainCommand (RegisterUser, userEmail))
import DomainEvent (DomainEvent (UserRegistered, email))
import EventRegistrator (insertEvent)
import Types.WithUuid (WithUuid (WithUuid, payload, uuid))

runExceptStateT :: s -> ExceptT e (StateT s m) a -> m (Either e a, s)
runExceptStateT = flip $ runStateT . runExceptT

----- here goes note taking aggregate, which is Context A
data ContextAState = ContextAState
  { registeredUserEmails :: Set Text,
    userNoteIds :: Map Int32 (Set Int32)
  }

-- type Aggregate a = DomainCommand -> ExceptStateT a [DomainEvent]

type ExceptStateT s = ExceptT CommandProcessingError (StateT s IO)

type PostgresState = ExceptT CommandProcessingError IO

class (Monad m) => Projection m where
  handleEvent :: DomainEvent -> m ()
  queryState :: m Bool

instance Projection (ExceptStateT ContextAState) where
  handleEvent event = modify $ fuckContext event

instance Projection PostgresState where
  handleEvent (UserRegistered email) = do
    let transformError :: PGError -> CommandProcessingError
        transformError _pgError = InternalError
    withExceptT transformError . ExceptT . try $
      -- event processing
      runQueryWithNewConnection_ [pgSQL| INSERT INTO users (email) VALUES (${email}); |]
  handleEvent _ = pure ()

data CommandValidationError
  = UserWithSuchEmailAlreadyExists
  | MaximumNoteLimitExceeded
  | NoteDoesNotExist
  deriving stock (Show)

data CommandProcessingError
  = InternalError
  | ValidationError CommandValidationError
  deriving stock (Show)

data EventProcessingError = EventProcessingError

processCommand' :: DomainCommand -> Either CommandProcessingError [DomainEvent]
processCommand' = undefined

-- gayProjection :: DomainEvent -> ContextAState -> ContextAState
-- gayProjection NoteCreated {noteId} state = state {userNoteIds = insert email registeredUserEmails}
-- gayProjection _ state = state

-- processCommand :: (Projection p, MonadIO p) => TBChan () -> DomainCommand -> p ()
processCommand :: TBChan () -> DomainCommand -> ExceptStateT ContextAState ()
processCommand eventsChan RegisterUser {userEmail} = do
  userAlreadyExists <- gets (\ContextAState {registeredUserEmails} -> userEmail `elem` registeredUserEmails)

  -- validate the command
  when userAlreadyExists $ throwError $ ValidationError UserWithSuchEmailAlreadyExists
  -- create events from the command
  let events = [UserRegistered userEmail]

  -- save events into the event log
  -- todo: wrap events into a transaction
  (mapExceptT liftIO . withExceptT (const InternalError)) $ mapM_ insertEvent events

  -- update the local validation projection state
  mapM_ (modify . fuckContext) events
  -- mapM_ handleEvent events

  -- notify on the events channel that new events are available
  liftIO $ atomically $ writeTBChan eventsChan ()
processCommand _ _ = pure ()

cuntProcessCommand :: TBChan () -> TChan (WithUuid DomainCommand) -> ExceptStateT ContextAState ()
cuntProcessCommand eventsChan commandsChan = do
  WithUuid uuid command <- liftIO $ atomically $ readTChan commandsChan
  liftIO $ putStrLn "Processing command..."
  -- todo: check for duplicate uuid before processing
  processCommand eventsChan command

processCommandsInLoop :: TChan (WithUuid DomainCommand) -> TBChan () -> ExceptStateT ContextAState ()
processCommandsInLoop commandsChan eventsChan = do
  WithUuid uuid command <- liftIO $ atomically $ readTChan commandsChan
  liftIO $ putStrLn "Processing command..."
  -- todo: check for duplicate uuid before processing
  processCommand eventsChan command

  processCommandsInLoop commandsChan eventsChan

runCommandProcessing :: ContextAState -> TBChan () -> TChan (WithUuid DomainCommand) -> IO ()
runCommandProcessing state eventsChan commandsChan =
  do
    (eitherRes, state') <- runExceptStateT state (cuntProcessCommand eventsChan commandsChan)
    case eitherRes of
      Right _ -> runCommandProcessing state' eventsChan commandsChan
      Left poop -> do
        print poop
        runCommandProcessing state' eventsChan commandsChan

-- эту хуйню дёргать будет command processor
fuckContext :: DomainEvent -> ContextAState -> ContextAState
fuckContext UserRegistered {email} ContextAState {registeredUserEmails} =
  ContextAState {registeredUserEmails = insert email registeredUserEmails}
fuckContext _ state = state

-- Use only in event processor
updateAccordingToEvent :: DomainEvent -> ExceptStateT ContextAState ()
updateAccordingToEvent event =
  modify $ fuckContext event
