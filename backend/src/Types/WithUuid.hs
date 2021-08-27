{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Types.WithUuid where

import Data.Aeson qualified as A
import Data.UUID qualified as UUID
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Language.Elm.Expression qualified as Expression
import Language.Elm.Type qualified as Type
import Language.Haskell.To.Elm qualified as E
import Types.Uuid ()

data WithUuid a = WithUuid {payload :: a, uuid :: UUID.UUID}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance E.HasElmType WithUuid where
  elmDefinition =
    Just $ E.deriveElmTypeDefinition @WithUuid E.defaultOptions "Api.WithUuid.WithUuid"

instance E.HasElmDecoder A.Value WithUuid where
  elmDecoderDefinition =
    Just $ E.deriveElmJSONDecoder @WithUuid E.defaultOptions A.defaultOptions "Api.WithUuid.decoder"

instance E.HasElmEncoder A.Value WithUuid where
  elmEncoderDefinition =
    Just $ E.deriveElmJSONEncoder @WithUuid E.defaultOptions A.defaultOptions "Api.WithUuid.encoder"

instance (E.HasElmType a) => E.HasElmType (WithUuid a) where
  elmType =
    Type.apps (E.elmType @WithUuid) [E.elmType @a]

instance (E.HasElmDecoder A.Value a) => E.HasElmDecoder A.Value (WithUuid a) where
  elmDecoder =
    Expression.apps (E.elmDecoder @A.Value @WithUuid) [E.elmDecoder @A.Value @a]

instance (E.HasElmEncoder A.Value a) => E.HasElmEncoder A.Value (WithUuid a) where
  elmEncoder =
    Expression.apps (E.elmEncoder @A.Value @WithUuid) [E.elmEncoder @A.Value @a]
