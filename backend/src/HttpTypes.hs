{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module HttpTypes where

import Data.Aeson qualified as Aeson
import Data.Int (Int32)
import Data.Text (Text)
import Data.UUID qualified as UUID
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Language.Elm.Definition (Definition)
import Language.Haskell.To.Elm qualified as E
import Utils.ElmDeriving (ElmType)

-- |
-- List of type definitions to be written to .elm files
-- Each new type from the domain model should be added there,
-- otherwise the root Elm module will fail to import some missing module,
-- or will refer to the type which definition was not written to file:
typeDefinitions :: [Definition]
typeDefinitions =
  concat
    [ E.jsonDefinitions @UserRegisterPayload,
      E.jsonDefinitions @Note
    ]

data WithUuid a = WithUuid {payload :: a, uuid :: UUID.UUID}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

newtype UserRegisterPayload = UserRegisterPayload {email :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (E.HasElmType, E.HasElmDecoder Aeson.Value, E.HasElmEncoder Aeson.Value)
    via ElmType "Api.UserRegisterPayload.UserRegisterPayload" UserRegisterPayload

newtype NoteCreatePayload = NoteCreatePayload {content :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (E.HasElmType, E.HasElmDecoder Aeson.Value, E.HasElmEncoder Aeson.Value)
    via ElmType "Api.NoteAddedPayload.NoteAddedPayload" NoteCreatePayload

data NoteUpdatePayload = NoteUpdatePayload {noteId :: Int32, content :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (E.HasElmType, E.HasElmDecoder Aeson.Value, E.HasElmEncoder Aeson.Value)
    via ElmType "Api.NoteAddedPayload.NoteAddedPayload" NoteUpdatePayload

data Note = Note {noteId :: Int32, content :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (E.HasElmType, E.HasElmDecoder Aeson.Value, E.HasElmEncoder Aeson.Value)
    via ElmType "Api.Note.Note" Note
