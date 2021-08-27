{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types.Uuid where

import Data.Aeson qualified as A
import Data.UUID qualified as UUID
import Language.Haskell.To.Elm qualified as E

instance E.HasElmType UUID.UUID where
  elmType =
    "Basics.String"

instance E.HasElmEncoder A.Value UUID.UUID where
  elmEncoder =
    "Json.Encode.string"

instance E.HasElmDecoder A.Value UUID.UUID where
  elmDecoder =
    "Json.Decode.string"
