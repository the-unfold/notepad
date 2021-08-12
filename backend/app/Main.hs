{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Connect (db)
import Control.Concurrent
import Control.Concurrent.Async (concurrently_)
import Control.Monad.Trans (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
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
import Data.HVect
import GHC.Records
import qualified Network.HTTP.Types as HttpTypes
import Web.Spock
  ( HasSpock (getState),
    SpockM,
    RouteSpec,
    Path,
    get,
    jsonBody,
    post,
    response,
    root,
    runSpock,
    setStatus,
    spock,
    text,
    var,
    (<//>), getContext, prehook, ActionCtxT
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
  putStrLn "poop str ln"
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
        _ -> fail "Json body expected. Status 400" -- TODO: fail with 400
  in 
    (\x -> (uuid x, eventFromPayload (payload x))) <$> resultOfPayload

-- huyFn :: (Path ('[]) Open) -> Int
-- huyFn p = 1

-- customHook :: ActionCtxT (HVect xs) (m) (HVect (Int '(:) xs))
-- customHook = do
--   oldCtx <- getContext
--   sess <- readSession
--   body <- jsonBody
--   case body of
--         Just validJson -> pure $ (42 :&: oldCtx)--Aeson.fromJSON @a validJson
--         _ -> undefined -- TODO:  setStatus HttpTypes.status400
--   -- mUser <- getUserFromSession

app :: SpockM () MySession () ()
app = do
  get root $ text "Hello World!"

  -- List all the events
  get "events" $ do
    evts <- liftIO listEvents
    case evts of
      Aeson.Error err -> setStatus HttpTypes.status500
      Aeson.Success xs -> text $ T.pack $ unwords $ map show xs

  -- Register user
  post "users" $ do
    body <- jsonBody
    let evtResult = makeEvent body registerUserFromPayload
    case evtResult of
      Aeson.Success (uuid, domainEvent) -> do
        liftIO $ insertEvent uuid domainEvent
        setStatus HttpTypes.status201
      _ -> setStatus HttpTypes.status400

  post "create-note" $ do
    body <- jsonBody
    let evtResult = makeEvent body (noteAddedFromPayload 1)
    case evtResult of 
      Aeson.Success  (uuid, domainEvent) -> do
        liftIO $ insertEvent uuid domainEvent
        setStatus HttpTypes.status201
      _ -> setStatus HttpTypes.status400

  post ("update-note" <//> var) $ \noteId -> do
    body <- jsonBody
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
      
  -- prehook (return (42 :: Int)) $ post "malone" $ do
  --   x <- getContext
  --   text "I've been fuckin' hoes and poppin' pillies \
  --        \Man, I feel just like a rockstar"
      