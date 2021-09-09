{-# LANGUAGE DerivingStrategies #-}

module DomainCommand (DomainCommand (..), CommandValidationError (..), CommandProcessingError (..)) where

import Data.Text (Text)
import DomainEvent (EventProcessingError)

data DomainCommand
  = RegisterUser {userEmail :: Text}
  | BringMeSomeBeer

data CommandValidationError
  = UserWithSuchEmailAlreadyExists
  | MaximumNoteLimitExceeded
  | NoteDoesNotExist
  deriving stock (Show)

data CommandProcessingError
  = InternalError
  | EventError EventProcessingError
  | ValidationError CommandValidationError
  deriving stock (Show)
