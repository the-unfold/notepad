{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module HttpTypes (typeDefinitions) where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.UUID qualified as UUID
import Utils.ElmDeriving (ElmType)
import Generics.SOP qualified as SOP
import GHC.Generics (Generic)
import Language.Elm.Definition (Definition)
import Language.Haskell.To.Elm qualified as E

-- |
-- List of type definitions to be written to .elm files
-- Each new type from the domain model should be added there,
-- otherwise the root Elm module will fail to import some missing module,
-- or will refer to the type which definition was not written to file:
typeDefinitions :: [Definition]
typeDefinitions =
  concat
    [ E.jsonDefinitions @RegisterUserPayload,
      E.jsonDefinitions @NoteAddedPayload
    ]

data WithUuid a = WithUuid {payload :: a, uuid :: UUID.UUID}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

newtype RegisterUserPayload = RegisterUserPayload {email :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving (E.HasElmType, E.HasElmDecoder Aeson.Value, E.HasElmEncoder Aeson.Value)
    via ElmType "Api.RegisterUserPayload.RegisterUserPayload" RegisterUserPayload

newtype NoteAddedPayload = NoteAddedPayload {content :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving (E.HasElmType, E.HasElmDecoder Aeson.Value, E.HasElmEncoder Aeson.Value)
    via ElmType "Api.NoteAddedPayload.NoteAddedPayload" NoteAddedPayload