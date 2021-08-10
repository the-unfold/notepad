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
import qualified Network.HTTP.Types as HttpTypes
import Web.Spock
  ( HasSpock (getState),
    SpockM,
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
    (<//>),
  )
import Web.Spock.Config
  ( PoolOrConn (PCNoDatabase),
    defaultSpockCfg,
  )

useTPGDatabase db -- compile time connection

-- | List of all our domain events in a single event log
data DomainEvent
  = UserRegistered {email :: Text}
  | TextChanged {updatedText :: Text}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data RegisterUserPayload = RegisterUserPayload {email :: Text, uuid :: UUID.UUID}
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

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

insertEvent :: UUID.UUID -> Aeson.Value -> PGSimpleQuery ()
insertEvent uuid body = [pgSQL| INSERT INTO events (uuid, body) VALUES (${uuid}, ${body}); |]

handleRequests :: IO ()
handleRequests =
  do
    ref <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase ()
    runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession () ()
app = do
  get root $ text "Hello World!"
  get "events" $ do
    evts <- liftIO listEvents
    case evts of
      Aeson.Error err -> setStatus HttpTypes.status500
      Aeson.Success xs -> text $ T.pack $ unwords $ map show xs
  post "events" $ do
    body <- jsonBody
    let resultOfPayload = case body of
          Just validJson -> Aeson.fromJSON @RegisterUserPayload validJson
          _ -> fail "Json body expected. Status 400" -- TODO: fail with 400
    let resultOfValue = fmap (Aeson.toJSON . (email :: RegisterUserPayload -> Text)) resultOfPayload
    let resultOfUuid = fmap (uuid :: RegisterUserPayload -> UUID.UUID) resultOfPayload
    case (resultOfValue, resultOfUuid) of
      (Aeson.Success value, Aeson.Success uuid) -> do
        liftIO $ runQuery $ insertEvent uuid value
        setStatus HttpTypes.status201
      _ -> setStatus HttpTypes.status400
