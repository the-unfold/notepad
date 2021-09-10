{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CommandProcessor (runCommandProcessing, runExceptStateT) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBChan (TBChan, tryWriteTBChan, writeTBChan)
import Control.Concurrent.STM.TChan (TChan, readTChan)
import Control.Monad.Except (ExceptT, mapExceptT, runExceptT, withExceptT)
import Control.Monad.State (StateT (runStateT))
import Control.Monad.Trans (MonadIO (liftIO))
import DomainCommand (CommandProcessingError (EventError, InternalError, ValidationError), DomainCommand)
import EventRegistrator (getAllEvents, insertEvent)
import Types.WithUuid (WithUuid (WithUuid))
import ValidationProjection (ValidationProjection, ValidationState, emptyValidationState, executeCommand, runValidationProjection, updateValidationProjection)

runExceptStateT :: s -> ExceptT e (StateT s m) a -> m (Either e a, s)
runExceptStateT = flip $ runStateT . runExceptT

----- here goes note taking aggregate, which is Context A
-- data ContextAState = ContextAState
--   { registeredUserEmails :: Set Text,
--     userNoteIds :: Map Int32 (Set Int32)
--   }

-- type Aggregate a = DomainCommand -> ExceptStateT a [DomainEvent]

-- type ExceptStateT s = ExceptT CommandProcessingError (StateT s IO)

-- type PostgresState = ExceptT CommandProcessingError IO

-- class (Monad m) => Projection m where
--   handleEvent :: DomainEvent -> m ()
--   queryState :: m Bool

-- instance Projection (ExceptStateT ContextAState) where
--   handleEvent event = modify $ fuckContext event

-- instance Projection PostgresState where
--   handleEvent (UserRegistered email) = do
--     let transformError :: PGError -> CommandProcessingError
--         transformError _pgError = InternalError
--     withExceptT transformError . ExceptT . try $
--       -- event processing
--       runQueryWithNewConnection_ [pgSQL| INSERT INTO users (email) VALUES (${email}); |]
--   handleEvent _ = pure ()

-- gayProjection :: DomainEvent -> ContextAState -> ContextAState
-- gayProjection NoteCreated {noteId} state = state {userNoteIds = insert email registeredUserEmails}
-- gayProjection _ state = state

-- processCommand :: (Projection p, MonadIO p) => TBChan () -> DomainCommand -> p ()
-- processCommand :: (Projection e m) => TBChan () -> DomainCommand -> m ()
-- processCommand eventsChan RegisterUser {userEmail} = do
--   userAlreadyExists <- gets (\ValidationState {registeredUserEmails} -> userEmail `elem` registeredUserEmails)

--   -- validate the command
--   when userAlreadyExists $ throwError $ ValidationError UserWithSuchEmailAlreadyExists
--   -- create events from the command
--   let events = [UserRegistered userEmail]

--   -- save events into the event log
--   -- todo: wrap events into a transaction
--   (mapExceptT liftIO . withExceptT (const InternalError)) $ mapM_ insertEvent events

--   -- update the local validation projection state
--   mapM_ (modify . fuckContext) events
--   -- mapM_ handleEvent events

--   -- notify on the events channel that new events are available
--   liftIO $ atomically $ writeTBChan eventsChan ()
-- processCommand _ _ = pure ()

runCommandProcessing :: TBChan () -> TChan (WithUuid DomainCommand) -> IO ()
runCommandProcessing eventsChan commandsChan = do
  -- initialize validation projection
  eventsResult <- runExceptT getAllEvents
  case eventsResult of
    Right events -> do
      -- todo: handle initialization error
      (processingResult, validationState) <- runValidationProjection emptyValidationState $ mapM_ updateValidationProjection events

      processCommandsRecursively validationState eventsChan commandsChan
    Left _ -> putStrLn "Failed to read event log"

processCommandsRecursively :: ValidationState -> TBChan () -> TChan (WithUuid DomainCommand) -> IO ()
processCommandsRecursively state eventsChan commandsChan = do
  -- take command from the command queue
  WithUuid uuid command <- liftIO $ atomically $ readTChan commandsChan

  (result, state') <- runValidationProjection state (processCommand eventsChan command)

  case result of
    Right () -> processCommandsRecursively state' eventsChan commandsChan
    Left err -> do
      print err
      processCommandsRecursively state' eventsChan commandsChan

processCommand :: TBChan () -> DomainCommand -> ValidationProjection CommandProcessingError ()
processCommand eventsChan command = do
  -- validate command and map to events
  events <- withExceptT ValidationError $ executeCommand command

  -- save events into event log
  (mapExceptT liftIO . withExceptT (const InternalError)) $ mapM_ insertEvent events

  -- notify that new events are available
  _written <- liftIO $ atomically $ tryWriteTBChan eventsChan ()

  -- apply events to the validation projection
  withExceptT EventError $ mapM_ updateValidationProjection events
