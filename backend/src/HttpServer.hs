{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module HttpServer where

import Web.Spock
  ( ActionCtxT,
    HasSpock (getState),
    Path,
    RouteSpec,
    SpockM,
    WebStateT,
    get,
    getContext,
    jsonBody,
    post,
    prehook,
    response,
    root,
    runSpock,
    setStatus,
    spock,
    static,
    text,
    var,
    (<//>),
  )

import Web.Spock.Config
  ( PoolOrConn (PCNoDatabase),
    defaultSpockCfg,
  )
import qualified Network.HTTP.Types as HttpTypes
import Web.Spock.SessionActions (readSession)
import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Int (Int16, Int32, Int64)
import Data.HVect
import Control.Monad.Trans (MonadIO (liftIO))
import Data.IORef (IORef, atomicModifyIORef', newIORef)

import qualified DomainEvent
import DomainEvent (DomainEvent)
import EventRegistrator

data MySession = EmptySession

data WithUuid a = WithUuid {payload :: a, uuid :: UUID.UUID}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

newtype RegisterUserPayload = RegisterUserPayload {email :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

newtype NoteAddedPayload = NoteAddedPayload {content :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

registerUserFromPayload :: RegisterUserPayload -> DomainEvent
registerUserFromPayload = DomainEvent.UserRegistered . (email :: RegisterUserPayload -> Text)

noteAddedFromPayload :: Int32 -> NoteAddedPayload -> DomainEvent
noteAddedFromPayload userId = DomainEvent.NoteAdded userId . (content :: NoteAddedPayload -> Text)

handleRequests :: IO ()
handleRequests =
  do
    ref <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase ()
    runSpock 8080 (spock spockCfg app)


makeEvent :: Aeson.FromJSON a => Maybe Aeson.Value -> (a -> b) -> Aeson.Result (UUID.UUID, b)
makeEvent body eventFromPayload =
  let resultOfPayload = case body of
        Just validJson -> Aeson.fromJSON validJson
        _ -> Aeson.Error "Json body expected."
   in (\x -> (uuid x, eventFromPayload (payload x))) <$> resultOfPayload

initHook :: Monad m => ActionCtxT () m (HVect '[])
initHook = pure HNil

-- Responsibility: decode client request body, transform to the DomainEvent
decodeBody :: (MonadIO m, Aeson.FromJSON a) => (a -> b) -> ActionCtxT (HVect xs) m (HVect (Aeson.Result (UUID.UUID, b) ': xs))
decodeBody eventFromPayload = do
  oldCtx <- getContext
  body <- jsonBody
  pure (makeEvent body eventFromPayload :&: oldCtx)

app :: SpockM () MySession () ()
app = prehook initHook $ do
  get root $ text "Hello World!"

  prehook (decodeBody registerUserFromPayload) $
    post "users" $ do
      contextVect <- getContext
      case Data.HVect.head contextVect of
        Aeson.Success (uuid, event) -> do
          liftIO $ insertEvent uuid event
          setStatus HttpTypes.status201
        _ -> setStatus HttpTypes.status400

  prehook (decodeBody (noteAddedFromPayload 1)) $
    post ("notes" <//> "create") $ do
      contextVect <- getContext
      case Data.HVect.head contextVect of
        Aeson.Success (uuid, event) -> do
          liftIO $ insertEvent uuid event
          setStatus HttpTypes.status201
        _ -> setStatus HttpTypes.status400

  post ("notes" <//> "update" <//> var) $ \noteId -> do
    body <- jsonBody
    context <- getContext
    let resultOfPayload = case body of
          Just validJson -> Aeson.fromJSON @(WithUuid NoteAddedPayload) validJson
          _ -> fail "Json body expected. Status 400"
    let resultOfContent = (\x -> content (payload x :: NoteAddedPayload)) <$> resultOfPayload
    let resultOfUuid = uuid <$> resultOfPayload
    case (resultOfContent, resultOfUuid) of
      (Aeson.Success content, Aeson.Success uuid) -> do
        liftIO $ insertEvent uuid (DomainEvent.NoteUpdated 1 noteId content)
        setStatus HttpTypes.status201
      _ -> setStatus HttpTypes.status400