module CommandProcessor (processCommandsInLoop) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBChan (TBChan, writeTBChan)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither, runExceptT, withExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (MonadState, StateT (runStateT), gets, modify)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Set (Set, insert)
import Data.Text (Text)
import DomainCommand (DomainCommand (RegisterUser, userEmail))
import DomainEvent (DomainEvent (UserRegistered, email))
import EventRegistrator (insertEvent')
import Types.WithUuid (WithUuid (WithUuid, payload, uuid))

runExceptionalMonad = runStateT . runExceptT

----- here goes note taking aggregate, which is Context A
newtype ContextAState = ContextAState {registeredUserEmails :: Set Text}

-- type Aggregate a = DomainCommand -> ExceptionalShitMonad a [DomainEvent]

-- ErrorType
data FuckYou = FuckYou

type ExceptionalShitMonad s = ExceptT FuckYou (StateT s IO)

data CommandProcessingError
  = CommandValidationFailed

processCommand :: TBChan () -> DomainCommand -> ExceptionalShitMonad ContextAState ()
processCommand eventsChan RegisterUser {userEmail} = do
  userAlreadyExists <- gets (\ContextAState {registeredUserEmails} -> userEmail `elem` registeredUserEmails)

  -- validate the command
  when userAlreadyExists $ throwError FuckYou
  -- create events from the command
  let events = [UserRegistered userEmail]

  -- save events into the event log
  -- todo: wrap events into a transaction
  res <- liftIO $ runExceptT $ mapM_ (withExceptT (const FuckYou) . insertEvent') events
  liftEither res

  -- update the local validation projection state
  mapM_ (modify . fuckContext) events

  -- notify on the events channel that new events are available
  liftIO $ atomically $ writeTBChan eventsChan ()
processCommand _ _ = pure ()

processCommandsInLoop :: TChan (WithUuid DomainCommand) -> TBChan () -> ExceptionalShitMonad ContextAState ()
processCommandsInLoop commandsChan eventsChan = do
  WithUuid uuid command <- liftIO $ atomically $ readTChan commandsChan
  liftIO $ putStrLn "Processing command..."
  -- todo: check for duplicate uuid before processing
  processCommand eventsChan command

  processCommandsInLoop commandsChan eventsChan

-- эту хуйню дёргать будет command processor
fuckContext :: DomainEvent -> ContextAState -> ContextAState
fuckContext UserRegistered {email} ContextAState {registeredUserEmails} =
  ContextAState {registeredUserEmails = insert email registeredUserEmails}
fuckContext _ state = state

-- Use only in event processor
updateAccordingToEvent :: DomainEvent -> ExceptionalShitMonad ContextAState ()
updateAccordingToEvent event =
  modify $ fuckContext event
