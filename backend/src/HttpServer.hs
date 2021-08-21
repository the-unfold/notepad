{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module HttpServer where

import Control.Concurrent.STM.TBChan (TBChan)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Aeson qualified as Aeson
import Data.HVect (HVect (..), head)
import Data.Int (Int32)
import Data.Text (Text)
import Data.UUID qualified as UUID
import DomainEvent (DomainEvent)
import DomainEvent qualified
import EventRegistrator (insertEvent)
import GHC.Generics (Generic)
import Network.HTTP.Types qualified as HttpTypes
import Web.Spock
  ( ActionCtxT,
    SpockM,
    get,
    getContext,
    jsonBody,
    post,
    prehook,
    root,
    runSpock,
    setStatus,
    spock,
    text,
    var,
    (<//>),
  )
import Web.Spock.Config
  ( PoolOrConn (PCNoDatabase),
    defaultSpockCfg,
  )

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

handleRequests :: TBChan () -> IO ()
handleRequests chan =
  do
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase ()
    runSpock 8080 (spock spockCfg $ app chan)

app :: TBChan () -> SpockM () MySession () ()
app chan = prehook initHook $ do
  get root $ text "Hello World!"

  prehook (decodeBody registerUserFromPayload) $
    post "users" $ do
      contextVect <- getContext
      case Data.HVect.head contextVect of
        Aeson.Success (uuid, event) -> do
          liftIO $ insertEvent chan uuid event
          setStatus HttpTypes.status201
        _ -> setStatus HttpTypes.status400

  prehook (decodeBody (noteAddedFromPayload 1)) $
    post ("notes" <//> "create") $ do
      contextVect <- getContext
      case Data.HVect.head contextVect of
        Aeson.Success (uuid, event) -> do
          liftIO $ insertEvent chan uuid event
          setStatus HttpTypes.status201
        _ -> setStatus HttpTypes.status400

  post ("notes" <//> "update" <//> var) $ \noteId -> do
    body <- jsonBody
    _context <- getContext
    let resultOfPayload = case body of
          Just validJson -> Aeson.fromJSON @(WithUuid NoteAddedPayload) validJson
          _ -> fail "Json body expected. Status 400"
    let resultOfContent = (\x -> content (payload x :: NoteAddedPayload)) <$> resultOfPayload
    let resultOfUuid = uuid <$> resultOfPayload
    case (resultOfContent, resultOfUuid) of
      (Aeson.Success content, Aeson.Success uuid) -> do
        liftIO $ insertEvent chan uuid (DomainEvent.NoteUpdated 1 noteId content)
        setStatus HttpTypes.status201
      _ -> setStatus HttpTypes.status400

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
