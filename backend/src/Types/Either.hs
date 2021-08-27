{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types.Either where

import Data.Aeson qualified as A
import Language.Elm.Expression qualified as Expression
import Language.Elm.Type qualified as Type
import Language.Haskell.To.Elm qualified as E

instance E.HasElmType Either where
  elmDefinition =
    Just $ E.deriveElmTypeDefinition @Either E.defaultOptions "Api.Either.Either"

instance E.HasElmDecoder A.Value Either where
  elmDecoderDefinition =
    Just $ E.deriveElmJSONDecoder @Either E.defaultOptions A.defaultOptions "Api.Either.decoder"

instance E.HasElmEncoder A.Value Either where
  elmEncoderDefinition =
    Just $ E.deriveElmJSONEncoder @Either E.defaultOptions A.defaultOptions "Api.Either.encoder"

instance (E.HasElmType a, E.HasElmType b) => E.HasElmType (Either a b) where
  elmType =
    Type.apps (E.elmType @Either) [E.elmType @a, E.elmType @b]

instance (E.HasElmDecoder A.Value a, E.HasElmDecoder A.Value b) => E.HasElmDecoder A.Value (Either a b) where
  elmDecoder =
    Expression.apps (E.elmDecoder @A.Value @Either) [E.elmDecoder @A.Value @a, E.elmDecoder @A.Value @b]

instance (E.HasElmEncoder A.Value a, E.HasElmEncoder A.Value b) => E.HasElmEncoder A.Value (Either a b) where
  elmEncoder =
    Expression.apps (E.elmEncoder @A.Value @Either) [E.elmEncoder @A.Value @a, E.elmEncoder @A.Value @b]
