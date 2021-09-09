{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ValidationProjection where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, when)
import Control.Monad.State (StateT, gets, modify, runStateT)
import Data.Int (Int32)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import DomainCommand (CommandValidationError (..), DomainCommand (RegisterUser), userEmail)
import DomainEvent (DomainEvent (UserRegistered), EventProcessingError, email)

type ValidationProjection e a = ExceptT e (StateT ValidationState IO) a

runValidationProjection :: ValidationState -> ValidationProjection e a -> IO (Either e a, ValidationState)
runValidationProjection = flip $ runStateT . runExceptT

data ValidationState = ValidationState
  { registeredUserEmails :: Set.Set Text,
    userNoteIds :: Map.Map Int32 (Set.Set Int32)
  }

insertUser :: Text -> ValidationState -> ValidationState
insertUser email prev =
  prev {registeredUserEmails = Set.insert email $ registeredUserEmails prev}

emptyValidationState :: ValidationState
emptyValidationState =
  ValidationState
    { registeredUserEmails = Set.empty,
      userNoteIds = Map.empty
    }

updateValidationProjection :: DomainEvent -> ValidationProjection EventProcessingError ()
updateValidationProjection UserRegistered {email} = modify (insertUser email)
updateValidationProjection _ = pure ()

executeCommand :: DomainCommand -> ValidationProjection CommandValidationError [DomainEvent]
executeCommand RegisterUser {userEmail} = do
  hasExistingUser <- gets (Set.member userEmail . registeredUserEmails)
  when hasExistingUser $ throwError UserWithSuchEmailAlreadyExists
  pure [UserRegistered userEmail]
executeCommand _ = pure []

-- instance Projection EventProcessingError (ExceptT EventProcessingError (StateT ValidationState IO)) where
--   update UserRegistered {email} = do
--     modify $ insertUser email
--   update _ = pure ()
