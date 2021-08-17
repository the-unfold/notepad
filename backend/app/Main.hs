{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Connect (db)
import Control.Concurrent
import Control.Concurrent.Async (concurrently_)
import Control.Monad.Trans (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import Data.HVect
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Database.PostgreSQL.Typed (pgConnect, pgDisconnect, useTPGDatabase)
import Database.PostgreSQL.Typed.Protocol (PGConnection)
import Database.PostgreSQL.Typed.Query (PGSimpleQuery, pgQuery, pgSQL)
import Database.PostgreSQL.Typed.Types (PGType)
import GHC.Generics (Generic)
import GHC.Records
import qualified Network.HTTP.Types as HttpTypes
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
import Web.Spock.SessionActions (readSession)

useTPGDatabase db -- compile time connection

-- | List of all our domain events in a single event log
data DomainEvent
  = UserRegistered {email :: Text}
  | NoteAdded {userId :: Int32, content :: Text}
  | NoteRemoved {userId :: Int32, noteId :: Int32}
  | NoteUpdated {userId :: Int32, noteId :: Int32, content :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

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
registerUserFromPayload = UserRegistered . (email :: RegisterUserPayload -> Text)

noteAddedFromPayload :: Int32 -> NoteAddedPayload -> DomainEvent
noteAddedFromPayload userId = NoteAdded userId . (content :: NoteAddedPayload -> Text)

data MySession = EmptySession

processEvents :: IO ()
processEvents = do
  -- process event according to its type (tag)
  -- save processing result (if any) to another table
  -- lastProcessedEventId
  -- let q = runQuery [pgSQL| SELECT event_id from last_processed_event LIMIT 1 |]

  -- Todo: 
  newEvents <- runQuery [pgSQL| 
    SELECT event_id, uuid, body, is_first
    FROM (
        SELECT e.event_id, uuid, body, FALSE as is_first
        FROM events e, last_processed_event
        WHERE e.event_id = last_processed_event.event_id + 1 AND EXISTS (SELECT 1 FROM last_processed_event )
      UNION
        SELECT e.event_id, uuid, body, TRUE as is_first
        FROM events e
        WHERE event_id = 1 AND NOT EXISTS (SELECT 1 FROM last_processed_event )) RESULTS
    ORDER BY event_id DESC
    LIMIT 1;
    |] :: IO [(Maybe Int32, Maybe UUID.UUID, Maybe Aeson.Value, Maybe Bool)]

  case newEvents of
    [(Just eventId, Just uuid, Just body, Just isFirst)] -> do
      putStrLn $ "processing event number " ++ show eventId 
      print (eventId, uuid, isFirst)
      if isFirst
        then runQuery [pgSQL| INSERT INTO last_processed_event (event_id) VALUES (${eventId}); |] 
        else runQuery [pgSQL| UPDATE last_processed_event SET event_id = ${eventId}; |] 
      pure ()
    _ -> putStrLn "waiting for events..."

  threadDelay 1000000
  processEvents

main :: IO ()
main =
  do
    concurrently_ processEvents handleRequests

runQuery :: PGSimpleQuery a -> IO [a]
runQuery q = do
  conn <- pgConnect db
  res <- pgQuery conn q
  pgDisconnect conn
  pure res

listEvents :: IO (Aeson.Result [DomainEvent])
listEvents = do
  traverse Aeson.fromJSON <$> (runQuery [pgSQL| SELECT body from events; |] :: IO [Aeson.Value])

insertEvent :: UUID.UUID -> DomainEvent -> IO [()]
insertEvent uuid body = runQuery [pgSQL| INSERT INTO events (uuid, body) VALUES (${uuid}, ${Aeson.toJSON body}); |]

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

decodeBody :: (MonadIO m, Aeson.FromJSON a) => (a -> b) -> ActionCtxT (HVect xs) m (HVect (Aeson.Result (UUID.UUID, b) ': xs))
decodeBody eventFromPayload = do
  oldCtx <- getContext
  body <- jsonBody
  pure (makeEvent body eventFromPayload :&: oldCtx)

app :: SpockM () MySession () ()
app = prehook initHook $ do
  get root $ text "Hello World!"

  -- List all the events
  get "events" $ do
    evts <- liftIO listEvents
    case evts of
      Aeson.Error err -> setStatus HttpTypes.status500
      Aeson.Success xs -> text $ T.pack $ unwords $ map show xs

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
        liftIO $ insertEvent uuid (NoteUpdated 1 noteId content)
        setStatus HttpTypes.status201
      _ -> setStatus HttpTypes.status400