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
import Network.HTTP.Types (status200, status500)
import Web.Spock
  ( HasSpock (getState),
    SpockM,
    get,
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
      Aeson.Error err -> setStatus status500
      Aeson.Success xs -> text $ T.pack $ unwords $ map show xs
  post "events" $ do
    liftIO $ case UUID.fromText "550e8400-e29b-41d4-a726-446655440000" of
      Just uuid -> runQuery $ insertEvent uuid (Aeson.toJSON $ UserRegistered "john@galt.com")
      _ -> pure [()]
    setStatus status200
