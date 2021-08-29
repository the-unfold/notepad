{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module HttpTypes where

import Data.Aeson qualified as A
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Language.Elm.Definition (Definition)
import Language.Haskell.To.Elm qualified as E
import Types.Either ()
import Types.WithUuid (WithUuid)
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
      E.jsonDefinitions @NoteCreatePayload,
      E.jsonDefinitions @NoteUpdatePayload,
      E.jsonDefinitions @Note,
      E.jsonDefinitions @Either,
      E.jsonDefinitions @WithUuid
    ]

newtype UserRegisterPayload = UserRegisterPayload {email :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (E.HasElmType, E.HasElmDecoder A.Value, E.HasElmEncoder A.Value)
    via ElmType "Api.UserRegisterPayload.UserRegisterPayload" UserRegisterPayload

newtype NoteCreatePayload = NoteCreatePayload {content :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (E.HasElmType, E.HasElmDecoder A.Value, E.HasElmEncoder A.Value)
    via ElmType "Api.NoteCreatePayload.NoteCreatePayload" NoteCreatePayload

data NoteUpdatePayload = NoteUpdatePayload {noteId :: Int32, content :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (E.HasElmType, E.HasElmDecoder A.Value, E.HasElmEncoder A.Value)
    via ElmType "Api.NoteUpdatePayload.NoteUpdatePayload" NoteUpdatePayload

data Note = Note {noteId :: Int32, content :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving (E.HasElmType, E.HasElmDecoder A.Value, E.HasElmEncoder A.Value) via ElmType "Api.Note.Note" Note
