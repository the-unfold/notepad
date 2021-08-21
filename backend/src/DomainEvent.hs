{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module DomainEvent where

import Data.Aeson qualified as Aeson
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | List of all our domain events in a single event log
data DomainEvent
  = UserRegistered {email :: Text}
  | NoteAdded {userId :: Int32, content :: Text}
  | NoteRemoved {userId :: Int32, noteId :: Int32}
  | NoteUpdated {userId :: Int32, noteId :: Int32, content :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)